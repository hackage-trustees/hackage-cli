{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      :  Main
-- Copyright   :  Herbert Valerio Riedel
-- License     :  GPL-3
--
module Main where

import qualified Blaze.ByteString.Builder              as Builder
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as BS8
import qualified Data.ByteString.Lazy                  as BSL
import qualified Data.ByteString.Search                as BSS
import           Data.Char                             (isSpace, toLower)
import qualified Data.List                             as List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock.POSIX                 (getPOSIXTime)
import           Data.Version                          (showVersion)
import qualified Distribution.Package                  as C
import qualified Distribution.PackageDescription       as C
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Verbosity                as C
import qualified Distribution.Version                  as C
import qualified Distribution.Text                     as C
import qualified Distribution.Simple.Utils             as C
import           Network.Http.Client
import           Network.NetRc
import           Numeric.Natural                       (Natural)
import           OpenSSL                               (withOpenSSL)
import           Options.Applicative                   as OA
import           System.Directory
import           System.Exit                           (exitFailure)
import           System.FilePath
import qualified System.IO.Streams                     as Streams
import           Text.HTML.TagSoup
import           Text.Printf                           (printf)
import qualified Paths_hackage_cli

-- import Cabal

import Distribution.Server.Util.CabalRevisions
import IndexShaSum

type PkgName = ByteString
type PkgVer  = ByteString
type PkgRev  = Word


pkgVerToVersion :: PkgVer -> C.Version
pkgVerToVersion = fromMaybe (error "invalid version") . C.simpleParse . BS8.unpack

pkgVerInRange :: PkgVer -> C.VersionRange -> Bool
pkgVerInRange v vr = pkgVerToVersion v `C.withinRange` vr

type HIO = StateT HConn IO

data HConn = HConn
    { _hcMkConn :: IO Connection
    , _hcConn   :: Maybe Connection
    , _hcReqCnt :: Natural -- ^ requests submitted on current 'Connection'
    , _hcRspCnt :: Natural -- ^ responses read from current 'Connection'
    }

makeLenses ''HConn

-- | Requests that can be issued in current connection before exhausting the 50-req/conn server limit
hcReqLeft :: Getter HConn Natural -- (Natural -> f Natural) -> HConn -> f HConn
hcReqLeft = hcReqCnt . to f
  where
    f n | n > lim   = 0
        | otherwise = lim - n
    lim = 50

setUA :: RequestBuilder ()
setUA = setHeader "User-Agent" uaStr
  where
    uaStr = "hackage-cli/" <> BS8.pack (showVersion Paths_hackage_cli.version)

hackageSendGET :: ByteString -> ByteString -> HIO ()
hackageSendGET p a = do
    q1 <- liftIO $ buildRequest $ do
        http GET p
        setUA
        setAccept a

    lft <- use hcReqLeft
    unless (lft > 0) $
        fail "hackageSendGET: request budget exhausted for current connection"

    c <- openHConn
    liftIO $ sendRequest c q1 emptyBody
    hcReqCnt += 1

hackagePutTgz :: ByteString -> ByteString -> HIO ByteString
hackagePutTgz p tgz = do
    q1 <- liftIO $ buildRequest $ do
        http PUT p
        setUA
        -- setAccept "application/json" -- wishful thinking
        setContentType "application/x-tar"
        -- setContentEncoding "gzip"
        setContentLength (fromIntegral $ BS.length tgz)

    lft <- use hcReqLeft
    unless (lft > 0) $
        fail "hackagePutTgz: request budget exhausted for current connection"

    c <- openHConn
    liftIO $ sendRequest c q1 (bsBody tgz)
    resp <- liftIO $ try (receiveResponse c concatHandler')
    closeHConn
    hcReqCnt += 1

    case resp of
        Right bs -> -- do
            -- liftIO $ BS.writeFile "raw.out" bs
            return bs

        Left e@HttpClientError {} -> -- do
            return (BS8.pack $ show e)

hackageRecvResp :: HIO ByteString
hackageRecvResp = do
    c <- openHConn
    reqCnt <- use hcReqCnt
    rspCnt <- use hcRspCnt
    unless (reqCnt > rspCnt) $
        fail "hackageRecvResp: not response available to receive"

    resp <- liftIO $ receiveResponse c concatHandler'
    hcRspCnt += 1

    return resp

data DryWetRun = DryRun | WetRun

hackagePostCabal :: (ByteString,ByteString) -> (PkgName,PkgVer) -> ByteString -> DryWetRun -> HIO ByteString
hackagePostCabal cred (pkgn,pkgv) rawcab dry = do
    when (boundary `BS.isInfixOf` rawcab) $ fail "WTF... cabal file contains boundary-pattern"

    q1 <- liftIO $ buildRequest $ do
        http POST urlpath
        setUA
        uncurry setAuthorizationBasic cred
        setAccept "application/json" -- wishful thinking
        setContentType ("multipart/form-data; boundary="<>boundary) -- RFC2388
        setContentLength bodyLen

    c <- reOpenHConn

    liftIO $ sendRequest c q1 (bsBody body)

    resp <- liftIO $ try (receiveResponse c concatHandler')
    closeHConn

    case resp of
        Right bs -> -- do
            -- liftIO $ BS.writeFile "raw.out" bs
            return (BS8.unlines [ h2 <> ":\n" <> renderTags ts | (h2, ts) <- scrape200 bs ])

        Left e@HttpClientError {} -> -- do
            -- Hackage currently timeouts w/ 503 guru meditation errors,
            -- which usually means that the transaction has succeeded
            -- liftIO $ BS.writeFile "raw.out" bs
            return (BS8.pack $ show e)
  where
    urlpath = mconcat [ "/package/", pkgn, "-", pkgv, "/", pkgn, ".cabal/edit" ]

    isDry DryRun = True
    isDry WetRun = False

    body = Builder.toByteString $ multiPartBuilder boundary
           [ ("cabalfile",[],[],rawcab)
           , if isDry dry
             then ("review", [],[],"Review changes")
             else ("publish",[],[],"Publish new revision")
           ]

    bodyLen = fromIntegral $ BS.length body

    boundary = "4d5bb1565a084d78868ff0178bdf4f61"

    -- scrape200 :: ByteString -> (Bool, h2parts)
    scrape200 html = h2parts
      where
        tags = parseTags (html :: ByteString)

        h2parts = [ (t,map cleanText $ takeWhile (/= TagClose "form") xs)
                  | (TagOpen "h2" _: TagText t: TagClose "h2": xs) <- partitions (== TagOpen "h2" []) tags
                  , t /= "Advice on adjusting version constraints" ]

        cleanText (TagText t)
          | t' == "", '\n' `BS8.elem` t = TagText "\n"
          | otherwise               = TagText t
          where
            t' = fst . BS8.spanEnd (=='\n') . BS8.dropWhile (=='\n') $ t
        cleanText x = x

bsBody :: ByteString -> Streams.OutputStream Builder.Builder -> IO ()
bsBody bs = Streams.write (Just (Builder.fromByteString bs))

-- | Upload a candidate to Hackage
--
-- This is a bit overkill, as one could easily just use @curl(1)@ for this:
--
-- > curl --form package=@"$PKGID".tar.gz -u "${CREDS}" https://hackage.haskell.org/packages/candidates/
--
hackagePushCandidate :: (ByteString,ByteString) -> (FilePath,ByteString) -> HIO ByteString
hackagePushCandidate cred (tarname,rawtarball) = do
    when (boundary `BS.isInfixOf` rawtarball) $ fail "WTF... tarball contains boundary-pattern"

    q1 <- liftIO $ buildRequest $ do
        http POST urlpath
        setUA
        uncurry setAuthorizationBasic cred
        setAccept "application/json" -- wishful thinking
        setContentType ("multipart/form-data; boundary="<>boundary) -- RFC2388
        setContentLength bodyLen

    c <- reOpenHConn

    liftIO $ sendRequest c q1 (bsBody body)

    resp <- liftIO $ try (receiveResponse c (\r is -> (,) r <$> concatHandler r is))
    closeHConn

    case resp of
        Right (rc,bs) -> do
            return (BS8.pack (show rc) <> bs)
        Left (HttpClientError code bs) -> return (BS8.pack ("code=" <> show code <> "\n") <> bs)
            -- Hackage currently timeouts w/ 503 guru meditation errors,
            -- which usually means that the transaction has succeeded
  where
    urlpath = "/packages/candidates/"

    body = Builder.toByteString $
           multiPartBuilder boundary [ ("package", [("filename", BS8.pack tarname)]
                                     , ["Content-Type: application/gzip"], rawtarball)]
    bodyLen = fromIntegral $ BS.length body

    boundary = "4d5bb1565a084d78868ff0178bdf4f61"

-- | Simplified RFC2388 multipart/form-data formatter
--
-- TODO: make a streaming-variant
multiPartBuilder :: ByteString -> [(ByteString,[(ByteString,ByteString)],[ByteString],ByteString)] -> Builder.Builder
multiPartBuilder boundary mparts = mconcat $ concatMap mkPart mparts ++ trailer
  where
    mkPart (name, xprops, xhdrs, payload)
        = [ dash, bs boundary, crlf
          , bs"Content-Disposition: form-data; name=\"", bs name, bs"\""
          ] ++
          concat [ [bs "; ", bs k, bs"=\"", bs v, bs"\"" ] | (k,v) <- xprops ] ++ [ crlf ] ++
          concat [ [bs h, crlf] | h <- xhdrs ] ++
          [ crlf
          , bs payload, crlf
          ]

    trailer = [ dash, bs boundary, dash, crlf ]

    crlf = bs"\r\n"
    dash = bs"--"
    bs = Builder.fromByteString

fetchVersions :: PkgName -> HIO [(PkgVer,PkgVerStatus)]
fetchVersions pkgn = do
    hackageSendGET ("/package/" <> pkgn) "text/html"
    resp <- hackageRecvResp
    liftIO $ evaluate $ scrapeVersions resp

fetchCabalFile :: PkgName -> PkgVer -> HIO ByteString
fetchCabalFile pkgn pkgv = do
    hackageSendGET urlpath "text/plain"
    hackageRecvResp
  where
    urlpath = mconcat ["/package/", pkgn, "-", pkgv, "/", pkgn, ".cabal"]

fetchCabalFiles :: PkgName -> [PkgVer] -> HIO [(PkgVer,ByteString)]
fetchCabalFiles pkgn pkgvs0 = do
    -- HTTP pipelining
    tmp <- go [] pkgvs0
    return (concat . reverse $ tmp)
  where
    go acc [] = pure acc
    go acc vs0 = do
        (_,lft) <- getHConn
        let (vs,vs') = nsplitAt lft vs0
        when (null vs) $ fail "fetchCabalFiles: the impossible happened"

        -- HTTP-pipeline requests; compensates a bit for SSL-induced latency
        mcabs <- forM (mkPipeline 4 vs) $ \case
            Left pkgv -> do -- request
                let urlpath = mconcat ["/package/", pkgn, "-", pkgv, "/", pkgn, ".cabal"]
                -- liftIO $ putStrLn $ show urlpath
                hackageSendGET urlpath "text/plain"
                return Nothing

            Right pkgv -> do -- response
                -- liftIO $ putStrLn ("read " ++ show pkgv)
                resp <- hackageRecvResp
                return $ Just (pkgv, resp)

        go (catMaybes mcabs : acc) vs'

-- Left means request; Right means receive
mkPipeline :: Natural -> [a] -> [Either a a]
mkPipeline maxQ vs
  | not postCond = error "mkPipeline: internal error" -- paranoia
  | otherwise    = concat [ map Left rqs1
                          , concat [ [Left v1, Right v2] | (v1,v2) <- zip rqs2 res2 ]
                          , map Right res3
                          ]
  where
    (rqs1,rqs2) = nsplitAt n vs
    (res2,res3) = nsplitAt m vs

    postCond = sameLen rqs2 res2 && sameLen rqs1 res3

    l = nlength vs
    n = min l maxQ
    m = l-n

    sameLen [] []         = True
    sameLen (_:xs) (_:ys) = sameLen xs ys
    sameLen [] (_:_)      = False
    sameLen (_:_) []      = False

-- | Insert or replace existing "x-revision" line
--
-- NOTE: Supports only simplified (i.e. without the @{;}@-layout) Cabal file grammar
cabalEditXRev :: PkgRev -> ByteString -> ByteString
cabalEditXRev xrev oldcab = BS8.unlines ls'
  where
    ls = BS8.lines oldcab

    xrevLine = "x-revision: " <> BS8.pack (show xrev) <> (if isCRLF then "\r" else "")

    -- | Try to detect if line contains the given field.
    -- I.e. try to match  @<field-name-ci> WS* ':' ...@
    matchFieldCI :: ByteString -> ByteString -> Bool
    matchFieldCI fname line
      | ':' `BS8.elem` line = fname == BS8.map toLower fname'
      | otherwise = False
      where
        fname' = fst . BS8.spanEnd isSpace . BS8.takeWhile (/=':') $ line

    -- simple heuristic
    isCRLF = case ls of
        []     -> False
        ("":_) -> False
        (l1:_) -> BS8.last l1 == '\r'

    ls' = case break (matchFieldCI "x-revision") ls of
        (_,[]) -> ls'' -- x-rev not found; try to insert after version-field instead
        (xs,_:ys) -> xs++ xrevLine:ys

    ls'' = case break (matchFieldCI "version") ls of
        (_,[]) -> error "cabalEditXRev: unsupported cabal grammar; version field not found"
        (xs,v:ys) -> xs ++ v:xrevLine:ys

fetchAllCabalFiles :: PkgName -> C.VersionRange -> HIO [(PkgVer,Maybe ByteString)]
fetchAllCabalFiles pkgn vrange = do
    vs <- fetchVersions pkgn
    liftIO $ putStrLn ("Found " ++ show (length vs) ++ " package versions for " ++ show pkgn ++ ", downloading now...")

    let (wanted,unwanted) = List.partition (`pkgVerInRange` vrange) (map fst vs)

    fetched <- map (fmap Just) <$> fetchCabalFiles pkgn wanted
    pure (List.sortOn (pkgVerToVersion . fst) (fetched ++ [ (v,Nothing) | v <- unwanted ]))

data PkgVerStatus = Normal | UnPreferred | Deprecated deriving Eq
instance NFData PkgVerStatus where rnf !_ = ()

scrapeVersions :: ByteString -> [(PkgVer,PkgVerStatus)]
scrapeVersions html = force vs
  where
    [vs] = mapMaybe getVerRow $ partitions (== TagOpen "tr" []) $ parseTags html

    getVerRow (TagOpen "tr" _ : TagOpen "th" _ : TagText "Versions" : TagClose "th" : TagOpen "td" _ : ts)
        | ts' <- trimVerRow ts = Just (map go $ chunksOf 4 ts')
    getVerRow _ = Nothing

    go [TagOpen "a" attr, TagText verStr, TagClose "a", TagText ", "] = (verStr,isPref attr)
    go [TagOpen "strong" attr, TagText verStr, TagClose "strong"] = (verStr,isPref attr)
    go _ = error "unexpected HTML structure structure"

    isPref as = case lookup "class" as of
        Just "unpreferred" -> UnPreferred
        Just "deprecated"  -> Deprecated
        Just _             -> error "unexpected version status"
        Nothing            -> Normal

    trimVerRow (reverse -> TagClose "tr":TagClose "td":
                           TagText ")":TagClose "a":TagText "info":TagOpen "a" _:TagText " (":ts') = reverse ts'
    trimVerRow (reverse -> TagClose "tr":TagClose "td":ts') = reverse ts'
    trimVerRow _ = error "trimVerRow: unexpected HTML structure"

closeHConn :: HIO ()
closeHConn = do
    mhc <- use hcConn
    forM_ mhc $ \hc -> do
        liftIO $ closeConnection hc
        hcConn   .= Nothing

        reqCnt <- use hcReqCnt
        rspCnt <- use hcRspCnt
        unless (reqCnt == rspCnt) $
            liftIO $ putStrLn $ concat ["warning: req-cnt=", show reqCnt, " rsp-cnt=", show rspCnt]

        hcReqCnt .= 0
        hcRspCnt .= 0

openHConn :: HIO Connection
openHConn = use hcConn >>= \case
    Just c -> return c
    Nothing -> do
        mkConn <- use hcMkConn
        c <- liftIO mkConn
        hcConn   .= Just c
        hcReqCnt .= 0 -- redundant
        hcRspCnt .= 0 -- redundant
        return c

reOpenHConn :: HIO Connection
reOpenHConn = closeHConn >> openHConn

getHConn :: HIO (Connection,Natural)
getHConn = do
    lft <- use hcReqLeft
    c <- if lft > 0 then openHConn else reOpenHConn
    (,) c <$> use hcReqLeft

nlength :: [a] -> Natural
nlength = fromIntegral . length

nsplitAt :: Natural -> [a] -> ([a],[a])
nsplitAt n = splitAt i
  where
    i = fromMaybe (error "nsplitAt: overflow") $ toIntegralSized n

----------------------------------------------------------------------------
-- CLI Interface

data Options = Options
  { optVerbose :: !Bool
  , optHost    :: !Hostname
  , optCommand :: !Command
  } deriving Show

data PullCOptions = PullCOptions
  { optPCPkgName :: PkgName
  , optPCPkgVers :: Maybe C.VersionRange
  } deriving Show

data ListCOptions = ListCOptions
  { optLCPkgName :: PkgName
  } deriving Show

data PushCOptions = PushCOptions
  { optPCIncrRev :: !Bool
  , optPCDry     :: !Bool
  , optPCFiles   :: [FilePath]
  } deriving Show

data PushPCOptions = PushPCOptions
  { optPPCFiles :: [FilePath]
  } deriving Show

data CheckROptions = CheckROptions
  { optCRNew  :: FilePath
  , optCROrig :: FilePath
  } deriving Show

data Command
    = ListCabal !ListCOptions
    | PullCabal !PullCOptions
    | PushCabal !PushCOptions
    | PushCandidate !PushPCOptions
    | CheckRevision !CheckROptions
    | IndexShaSum   !IndexShaSumOptions
    deriving Show

optionsParserInfo :: ParserInfo Options
optionsParserInfo
    = info (helper <*> verOption <*> oParser)
           (fullDesc
            <> header "hackage-cli - CLI tool for Hackage"
            <> footer "\
              \ Each command has a sub-`--help` text. Hackage credentials are expected to be \
              \ stored in an `${HOME}/.netrc`-entry for the respective Hackage hostname. \
              \ E.g. \"machine hackage.haskell.org login MyUserName password TrustNo1\". \
              \ All interactions with Hackage occur TLS-encrypted via the HTTPS protocol. \
              \ ")

  where
    bstr = BS8.pack <$> str

    vrange = do
        s <- str
        case C.simpleParse s of
            Nothing -> fail "invalid version range"
            Just vr -> pure vr

    listcoParser = ListCabal . ListCOptions <$> OA.argument bstr (metavar "PKGNAME")
    pullcoParser = PullCabal <$>
        (PullCOptions <$> OA.argument bstr (metavar "PKGNAME")
                      <*> optional (OA.argument vrange (metavar "VERSION-CONSTRAINT")))
    pushcoParser = PushCabal <$> (PushCOptions
                                  <$> switch (long "incr-rev" <> help "increment x-revision field")
                                  <*> switch (long "dry"      <> help "upload in review-mode")
                                  <*> some (OA.argument str (metavar "CABALFILES...")))

    pushpcoParser = PushCandidate <$> (PushPCOptions <$> some (OA.argument str (metavar "TARBALLS...")))

    checkrevParsser = CheckRevision <$> (CheckROptions <$> OA.argument str (metavar "NEWCABAL")
                                                       <*> OA.argument str (metavar "OLDCABAL"))


    indexssParser = IndexShaSum <$> (IndexShaSumOptions <$> switch (long "flat" <> help "flat filesystem layout (used by mirrors)")
                                                        <*> OA.argument str (metavar "INDEX-TAR")
                                                        <*> optional (OA.argument str (metavar "BASEDIR")))

    oParser
        = Options <$> switch (long "verbose" <> help "enable verbose output")
                  <*> option bstr (long "hostname"  <> metavar "HOSTNAME" <> value "hackage.haskell.org"
                                   <> help "Hackage hostname" <> showDefault)
                  <*> subparser (mconcat [ command "pull-cabal" (info (helper <*> pullcoParser)
                                                   (progDesc "download .cabal files for a package"))
                                         , command "push-cabal" (info (helper <*> pushcoParser)
                                                   (progDesc "upload revised .cabal files"))
                                         , command "push-candidate" (info (helper <*> pushpcoParser)
                                                   (progDesc "upload package candidate(s)"))
                                         , command "list-versions" (info (helper <*> listcoParser)
                                                   (progDesc "list versions for a package"))
                                         , command "check-revision" (info (helper <*> checkrevParsser)
                                                   (progDesc "validate revision"))
                                         , command "index-sha256sum" (info (helper <*> indexssParser)
                                                   (progDesc "generate sha256sum-format file"))
                                         ])

    verOption = infoOption verMsg (long "version" <> help "output version information and exit")
      where
        verMsg = "hackage-cli " <> showVersion Paths_hackage_cli.version

----------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- execParser optionsParserInfo
    withOpenSSL (mainWithOptions opts)

mainWithOptions :: Options -> IO ()
mainWithOptions Options {..} = do
   case optCommand of
       PullCabal (PullCOptions {..}) -> do
           let pkgn = optPCPkgName

           cs <- runHConn (fetchAllCabalFiles pkgn (fromMaybe C.anyVersion optPCPkgVers))

           forM_ cs $ \(v,mraw) -> case mraw of
             Nothing -> putStrLn ("skipped excluded " ++ BS8.unpack v)
             Just raw -> do
               let fn = BS8.unpack $ pkgn <> "-" <> v <> ".cabal"

               doesFileExist fn >>= \case
                   False -> do
                       BS.writeFile fn raw
                       putStrLn ("saved " ++ fn ++ " (" ++ show (BS.length raw) ++ " bytes)")
                   True ->
                       putStrLn ("WARNING: skipped existing " ++ fn)

           return ()

       ListCabal (ListCOptions {..}) -> do
           let pkgn = optLCPkgName

           vs <- runHConn (fetchVersions pkgn)

           putStrLn $ concat [ "Found ", show (length vs), " package versions for "
                             , show pkgn, " ([U]npreferred, [D]eprecated):"
                             ]

           forM_ vs $ \(v,unp) -> do
               let status = case unp of
                       Normal      -> "    "
                       Deprecated  -> "[D] "
                       UnPreferred -> "[U] "
               BS8.putStrLn $ status <> pkgn <> "-" <> v
           return ()

       PushCabal (PushCOptions {..}) -> do
           (username,password) <- maybe (fail "missing Hackage credentials") return =<< getHackageCreds
           putStrLn $ "Using Hackage credentials for username " ++ show username

           forM_ optPCFiles $ \fn -> do
               (pkgn,pkgv,xrev) <- pkgDescToPkgIdXrev <$> C.readPackageDescription C.deafening fn
               putStrLn $ concat [ "Pushing ", show fn
                                 , " (", BS8.unpack pkgn, "-", BS8.unpack pkgv, "~", show xrev, ")"
                                 , if optPCDry then " [review-mode]" else "", " ..."
                                 ]

               let editCab | optPCIncrRev = cabalEditXRev (xrev+1)
                           | otherwise    = id
               rawcab <- editCab <$> BS.readFile fn
               (dt,tmp) <- timeIt $ runHConn (hackagePostCabal (username,password) (pkgn,pkgv) rawcab
                                                               (if optPCDry then DryRun else WetRun))

               printf "Hackage response was (after %.3f secs):\n" dt
               putStrLn (replicate 80 '=')
               BS8.putStrLn (tidyHtml tmp)
               putStrLn (replicate 80 '=')

       PushCandidate (PushPCOptions {..}) -> do
           (username,password) <- maybe (fail "missing Hackage credentials") return =<< getHackageCreds
           putStrLn $ "Using Hackage credentials for username " ++ show username

           forM_ optPPCFiles $ \fn -> do
               putStrLn $ "reading " ++ show fn ++ " ..."
               rawtar <- BS.readFile fn
               putStrLn $ "uplading to Hackage..."
               tmp <- runHConn (hackagePushCandidate (username,password) (takeFileName fn, rawtar))

               putStrLn "Hackage response was:"
               putStrLn (replicate 80 '=')
               BS8.putStrLn tmp
               putStrLn (replicate 80 '=')


       CheckRevision (CheckROptions {..}) -> do
           old <- C.readUTF8File optCROrig
           new <- C.readUTF8File optCRNew

           case diffCabalRevisions old new of
               Left err -> do
                   putStrLn "change not allowed:"
                   putStrLn err
                   exitFailure

               Right [] -> do
                   putStrLn "no-op change detected"
                   exitFailure

               Right changes -> do
                   putStrLn "change allowed:"
                   forM_ changes $ \(Change what old' new') -> do
                       putStrLn $ "what: " ++ what
                       putStrLn $ " old: " ++ old'
                       putStrLn $ " new: " ++ new'

           return ()

       IndexShaSum opts -> IndexShaSum.run opts


   return ()
  where
    mkHConn = do
        sslCtx <- baselineContextSSL
        pure $ HConn (openConnectionSSL sslCtx optHost 443) Nothing 0 0

    runHConn act = do
        hc0 <- mkHConn
        flip evalStateT hc0 $ do
            res <- act
            closeHConn
            return res

    getHackageCreds :: IO (Maybe (ByteString,ByteString))
    getHackageCreds =
        readUserNetRc >>= \case
            Nothing -> pure Nothing
            Just (Left _) -> fail "Invalid ${HOME}/.netrc found"
            Just (Right (NetRc {..})) ->
                evaluate $ (\NetRcHost{..} -> (nrhLogin,nrhPassword))
                           <$> listToMaybe (filter ((== optHost) . nrhName) nrHosts)


    pkgDescToPkgIdXrev pdesc = force (BS8.pack pkgn, BS8.pack $ showVersion pkgv, read xrev :: PkgRev)
      where
        C.PackageIdentifier (C.PackageName pkgn) pkgv = C.package . C.packageDescription $ pdesc
        xrev = fromMaybe "0" . lookup "x-revision" . C.customFieldsPD . C.packageDescription $ pdesc

-- | Try to clean-up HTML fragments to be more readable
tidyHtml :: ByteString -> ByteString
tidyHtml =
    replace "&amp;"  "&"     . -- must be last entity substitution
    replace "&gt;"   ">"     .
    replace "&lt;"   "<"     .
    replace "<p>"    ""      . -- tags must be replaced before entities
    replace "</p>"   "\n"    .
    replace "<li>"   "\n * " .
    replace "</li>"  ""      .
    replace "</pre>" "`"     .
    replace "<pre>"  "`"     .
    stripEnd
  where
    stripEnd = fst . BS8.spanEnd isSpace

    replace :: ByteString -> ByteString -> ByteString -> ByteString
    replace old new = BSL.toStrict . BSS.replace old new


timeIt :: IO a -> IO (Double, a)
timeIt act = do
    t0  <- getTime
    res <- act
    t1  <- getTime
    let !dt = t1 - t0
    pure (dt, res)
  where
    getTime :: IO Double
    getTime = realToFrac `fmap` getPOSIXTime
