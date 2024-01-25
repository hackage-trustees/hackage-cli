{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      :  Main
-- Copyright   :  Herbert Valerio Riedel, Andreas Abel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Main where

import           Prelude                                hiding (log)

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Types                       as J
import qualified Data.ByteString.Builder                as Builder
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except                   (MonadError(..), ExceptT, runExceptT)
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BS8
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.ByteString.Search                 as BSS
import           Data.Char                              (isSpace)
import           Data.Foldable                          (toList)
import           Data.Function.Compat                   (applyWhen)
import qualified Data.List                              as List
import           Data.Maybe
import           Data.Time.Clock.POSIX                  (getPOSIXTime)
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup                         (Semigroup(..))
#endif

import qualified Distribution.Package                   as C
import qualified Distribution.PackageDescription        as C
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Parsec                    as C
import qualified Distribution.Fields                    as C
import qualified Distribution.Fields.Field              as C (fieldAnn)
import qualified Distribution.Pretty                    as C
import qualified Distribution.Verbosity                 as C
import qualified Distribution.Version                   as C
import qualified Distribution.Text                      as C
#if MIN_VERSION_Cabal(3,7,0)
import qualified Distribution.Simple.PackageDescription as C
#endif

import           Lens.Micro
import           Lens.Micro.Mtl
import           Lens.Micro.TH

import           Network.Http.Client
import           Network.NetRc
import           Numeric.Natural                        (Natural)
import           Options.Applicative                    as OA
import           System.Directory
import           System.Environment                     (lookupEnv)
import           System.Exit                            (ExitCode (..), exitFailure)
import           System.FilePath
import           System.IO                              (hPutStrLn, stderr)
import           System.IO.Error                        (tryIOError, isDoesNotExistError)
import qualified System.IO.Streams                      as Streams
import           System.Process.ByteString              (readProcessWithExitCode)
import           Text.HTML.TagSoup
import           Text.Printf                            (printf)
import qualified Paths_hackage_cli
import qualified Data.Version as V

import qualified Distribution.Types.BuildInfo.Lens                 as LC
import qualified Distribution.Types.GenericPackageDescription.Lens as LC

-- import Cabal

import Distribution.Server.Util.CabalRevisions
import IndexShaSum
import CabalEdit

type PkgName = ByteString
type PkgVer  = ByteString

showVersion :: C.Version -> String
showVersion = C.display

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
hcReqLeft :: SimpleGetter HConn Natural -- (Natural -> f Natural) -> HConn -> f HConn
hcReqLeft = hcReqCnt . to f
  where
    f n | n > lim   = 0
        | otherwise = lim - n
    lim = 50

setUA :: RequestBuilder ()
setUA = setHeader "User-Agent" uaStr
  where
    uaStr = "hackage-cli/" <> BS8.pack (V.showVersion Paths_hackage_cli.version)

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

-- | Whether to operate in review/test mode or publish the revision for real
data DryWetRun = DryRun -- ^ review/test mode
               | WetRun -- ^ publish

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

    body = Builder.toLazyByteString $ multiPartBuilder boundary
           [ ("cabalfile",[],[],rawcab)
           , if isDry dry
             then ("review", [],[],"Review changes")
             else ("publish",[],[],"Publish new revision")
           ]

    bodyLen = fromIntegral $ BSL.length body

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

class ToBuilder a where
  toBuilder :: a -> Builder.Builder

instance ToBuilder ByteString where
  toBuilder = Builder.byteString

instance ToBuilder BSL.ByteString where
  toBuilder = Builder.lazyByteString

bsBody :: ToBuilder a => a -> Streams.OutputStream Builder.Builder -> IO ()
bsBody bs = Streams.write (Just (toBuilder bs))

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

    body = Builder.toLazyByteString $
           multiPartBuilder boundary [ ("package", [("filename", BS8.pack tarname)]
                                     , ["Content-Type: application/gzip"], rawtarball)]
    bodyLen = fromIntegral $ BSL.length body

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
    bs = Builder.byteString

fetchVersions :: PkgName -> HIO [(PkgVer,PkgVerStatus)]
fetchVersions pkgn = do
    hackageSendGET ("/package/" <> pkgn <> "/preferred") "application/json"
    resp <- hackageRecvResp
    case decodeVersions resp of
        Right xs -> pure xs
        Left err -> fail err

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

fetchAllCabalFiles :: PkgName -> C.VersionRange -> HIO [(PkgVer,Maybe ByteString)]
fetchAllCabalFiles pkgn vrange = do
    vs <- fetchVersions pkgn
    liftIO $ putStrLn ("Found " ++ show (length vs) ++ " package versions for " ++ show pkgn ++ ", downloading now...")

    let (wanted,unwanted) = List.partition (`pkgVerInRange` vrange) (map fst vs)

    fetched <- map (fmap Just) <$> fetchCabalFiles pkgn wanted
    pure (List.sortOn (pkgVerToVersion . fst) (fetched ++ [ (v,Nothing) | v <- unwanted ]))

data PkgVerStatus = Normal | UnPreferred | Deprecated deriving (Eq,Show)
instance NFData PkgVerStatus where rnf !_ = ()

decodeVersions :: ByteString -> Either String [(PkgVer,PkgVerStatus)]
decodeVersions bs = do
    obj <- J.eitherDecode' (BSL.fromStrict bs)
    flip J.parseEither obj $ \o -> do
        normal <- o J..:? "normal-version"
        unpreferred <- o J..:? "unpreferred-version"
        deprecated <- o J..:? "deprecated-version"
        pure $ toPairs Normal normal ++ toPairs UnPreferred unpreferred ++ toPairs Deprecated deprecated
  where
    toPairs :: PkgVerStatus -> Maybe [String] -> [(PkgVer,PkgVerStatus)]
    toPairs s (Just vs) = [(BS8.pack v, s) | v <- vs]
    toPairs _ _ = []

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

-- | Returns the position @(line, indentation)@ where to insert the
-- new @build-depends:@ immediately after the first @library@ section,
-- if any.
--
findLibrarySection :: [C.Field C.Position] -> Maybe (Int, Int)
findLibrarySection [] = Nothing
findLibrarySection (C.Section (C.Name (C.Position row _) "library") [] fs : _) =
    Just (row, findIndent fs)
  where
    findIndent [] = 4
    findIndent (f : _) = case C.fieldAnn f of
        C.Position _ col -> pred col
findLibrarySection (_ : fs) = findLibrarySection fs

----------------------------------------------------------------------------
-- CLI Interface

data Options = Options
  { optVerbose :: !Bool
  , optHost    :: !Hostname
  , optCommand :: !Command
  } deriving Show

data PullCOptions = PullCOptions
  { optPlCPkgName :: !PkgName
  , optPlCIncrRev :: !Bool
  , optPlCForce   :: !Bool
  , optPlCPkgVers :: Maybe C.VersionRange
  } deriving Show

data SyncCOptions = SyncCOptions
  { optSyCFile    :: FilePath
  , optSyCIncrRev :: !Bool
  , optSyCForce   :: !Bool
  } deriving Show

data ListCOptions = ListCOptions
  { optLCPkgName :: !PkgName
  , optNoAnn     :: !Bool
  , optRevUrls   :: !Bool
  } deriving Show

data PushCOptions = PushCOptions
  { optPsCIncrRev :: !Bool
  , optPsCPublish :: !Bool
  , optPsCFiles   :: [FilePath]
  } deriving Show

data PushPCOptions = PushPCOptions
  { optPPCFiles :: [FilePath]
  } deriving Show

data CheckROptions = CheckROptions
  { optCRNew  :: FilePath
  , optCROrig :: FilePath
  } deriving Show

type AddBoundOptions = AddBoundOptions' [FilePath]
data AddBoundOptions' a = AddBoundOptions
  { optABPackageName  :: C.PackageName
  , optABVersionRange :: C.VersionRange
  , optForce          :: Bool              -- ^ Disable the check whether bound is subsumed by existing constraints.
  , optABMessage      :: [String]
  , optABFiles        :: a                 -- ^ One or several files.
  } deriving (Show, Functor)

data Command
    = ListCabal !ListCOptions
    | PullCabal !PullCOptions
    | PushCabal !PushCOptions
    | SyncCabal !SyncCOptions
    | PushCandidate !PushPCOptions
    | CheckRevision !CheckROptions
    | IndexShaSum   !IndexShaSumOptions
    | AddBound !AddBoundOptions
    deriving Show

optionsParserInfo :: ParserInfo Options
optionsParserInfo
    = info (helper <*> verOption <*> oParser)
           (fullDesc
            <> header "hackage-cli - CLI tool for Hackage"
            <> footer footerStr)
  where
    footerStr = unwords
        [ "Each command has a sub-`--help` text. Hackage credentials are expected to be"
        , "stored in an `${HOME}/.netrc`-entry (or `.netrc.gpg`) for the respective Hackage hostname."
        , "E.g. \"machine hackage.haskell.org login MyUserName password TrustNo\"."
        , "All interactions with Hackage occur TLS-encrypted via the HTTPS protocol."
        ]

    bstr = BS8.pack <$> str

    prsc :: C.Parsec s => OA.ReadM s
    prsc = OA.eitherReader C.eitherParsec

    vrange = do
        s <- str
        case C.simpleParse s of
            Nothing -> fail "invalid version range"
            Just vr -> pure vr

    listcoParser = ListCabal <$>
        (ListCOptions <$> OA.argument bstr (metavar "PKGNAME" <> action "file")
                      <*> switch (long "no-annotations" <> help "don't add preferred-versions annotations")
                      <*> switch (long "rev-urls" <> help "list revision URLs"))

    pullcoParser = PullCabal <$>
        (PullCOptions <$> OA.argument bstr (metavar "PKGNAME" <> action "file")
                      <*> switch (long "incr-rev" <> help "increment x-revision field")
                      <*> switch (long "force" <> help "force overwriting existing files")
                      <*> optional (OA.argument vrange (metavar "VERSION-CONSTRAINT")))

    synccoParser = SyncCabal <$>
        (SyncCOptions <$> OA.argument str (metavar "CABALFILE" <> action "file")
                      <*> switch (long "incr-rev" <> help "increment x-revision field")
                      <*> switch (long "force" <> help "force overwriting local file with older revision"))

    pushcoParser = PushCabal <$> (PushCOptions
                             <$> switch (long "incr-rev" <> help "increment x-revision field")
                             <*> switch (long "publish"  <> help "publish revision (review-mode)")
                             <*> some (OA.argument str (metavar "CABALFILES..." <> action "file")))

    pushpcoParser = PushCandidate <$> (PushPCOptions <$> some (OA.argument str (metavar "TARBALLS..." <> action "file")))

    checkrevParsser = CheckRevision <$> (CheckROptions <$> OA.argument str (metavar "NEWCABAL" <> action "file")
                                                       <*> OA.argument str (metavar "OLDCABAL" <> action "file"))


    indexssParser = IndexShaSum <$> (IndexShaSumOptions <$> switch (long "flat" <> help "flat filesystem layout (used by mirrors)")
                                                        <*> OA.argument str (metavar "INDEX-TAR" <> action "file")
                                                        <*> optional (OA.argument str (metavar "BASEDIR" <> action "directory")))

    addboundParser = AddBound <$> (AddBoundOptions <$> OA.argument prsc (metavar "DEPENDENCY")
                                                   <*> OA.argument prsc (metavar "VERSIONRANGE")
                                                   <*> OA.switch (long "force" <> help "Add bound even if it is already subsumed by existing constraints.")
                                                   <*> many (OA.option str (OA.short 'm' <> OA.long "message" <> metavar "MSG" <> help "Use given MSG as a comment. If multiple -m options are given, their values are concatenated with 'unlines'."))
                                                   <*> some (OA.argument str (metavar "CABALFILES..." <> action "file")))

    oParser
        = Options <$> switch (long "verbose" <> help "Enable verbose output.")
                  <*> option bstr (long "hostname"  <> metavar "HOSTNAME" <> value "hackage.haskell.org"
                                   <> help "Hackage hostname" <> showDefault)
                  <*> subparser (mconcat [ command "pull-cabal" (info (helper <*> pullcoParser)
                                                   (progDesc "Download .cabal files for a package."))
                                         , command "push-cabal" (info (helper <*> pushcoParser)
                                                   (progDesc "Upload revised .cabal files."))
                                         , command "sync-cabal" (info (helper <*> synccoParser)
                                                   (progDesc "Update/sync local .cabal file with latest revision on Hackage."))
                                         , command "push-candidate" (info (helper <*> pushpcoParser)
                                                   (progDesc "Upload package candidate(s)."))
                                         , command "list-versions" (info (helper <*> listcoParser)
                                                   (progDesc "List versions for a package."))
                                         , command "check-revision" (info (helper <*> checkrevParsser)
                                                   (progDesc "Validate revision."))
                                         , command "index-sha256sum" (info (helper <*> indexssParser)
                                                   (progDesc "Generate sha256sum-format file."))
                                         , command "add-bound" (info (helper <*> addboundParser)
                                                   (progDesc "Add bound to the library section of a package, unless the bound is redundant. The .cabal file is edited in place."))
                                         ])

    verOption = infoOption verMsg (long "version" <> help "Output version information and exit.")
      where
        verMsg = "hackage-cli " <> V.showVersion Paths_hackage_cli.version

----------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- execParser optionsParserInfo
    mainWithOptions opts

mainWithOptions :: Options -> IO ()
mainWithOptions Options {..} = do
   case optCommand of
       PullCabal (PullCOptions {..}) -> do
           let pkgn = optPlCPkgName

           cs <- runHConn (fetchAllCabalFiles pkgn (fromMaybe C.anyVersion optPlCPkgVers))

           forM_ cs $ \(v,mraw) -> case mraw of
             Nothing -> putStrLn ("skipped excluded " ++ BS8.unpack v)
             Just raw0 -> do
               let fn = BS8.unpack $ pkgn <> "-" <> v <> ".cabal"

               let raw | optPlCIncrRev = incrXrev raw0
                       | otherwise     = raw0

               doesFileExist fn >>= \case
                   False -> do
                       BS.writeFile fn raw
                       putStrLn ("saved " ++ fn ++ " (" ++ show (BS.length raw) ++ " bytes)")
                   True ->
                       if optPlCForce
                       then do
                         BS.writeFile fn raw
                         putStrLn ("overwritten " ++ fn ++ " (" ++ show (BS.length raw) ++ " bytes)")
                       else
                         putStrLn ("WARNING: skipped existing " ++ fn ++ " (use --force to overwrite)")

           return ()

       SyncCabal (SyncCOptions {..}) -> do
           (pkgn,pkgv,xrev) <- pkgDescToPkgIdXrev <$> C.readGenericPackageDescription C.deafening optSyCFile
           cab0 <- BS.readFile optSyCFile

           BS8.putStrLn $ mconcat [ "local :  "
                                  , pkgn, "-", pkgv, "-r", BS8.pack (show xrev)
                                  , "   ('", BS8.pack optSyCFile, "')"
                                  ]

           cab' <- runHConn (fetchCabalFile pkgn pkgv)

           let (pkgn',pkgv',xrev') = pkgDescToPkgIdXrev $ parseGenericPackageDescription' cab'

           BS8.putStrLn $ mconcat [ "remote:  "
                                  , pkgn', "-", pkgv', "-r", BS8.pack (show xrev')
                                  ]

           let cab'' = cabalEditXRev (xrev'+1) cab'
               bakfn = optSyCFile <> "~"

           case () of
             _ | optSyCIncrRev, cab'' == cab0 -> do
                     putStrLn "INFO: local and (incremented) latest remote .cabal revision are already identical! Nothing to do."

             _ | not optSyCForce, xrev' < xrev -> do
                     putStrLn "ERROR: Local file has higher revision number than Hackage - aborting! (use --force to allow this)"
                     exitFailure

             _ | optSyCIncrRev, cab'' /= cab0 -> do
                     when (cab' == cab0) $ do
                         putStrLn "NOTE: local and (non-incremented) latest remote .cabal revision are identical."

                     BS.writeFile bakfn cab0
                     putStrLn ("INFO: saved backup of original local file to " <> bakfn)

                     BS.writeFile optSyCFile cab''
                     BS8.putStrLn $ mconcat [ "local :  "
                                            , pkgn, "-", pkgv, "-r", BS8.pack (show $ xrev'+1)
                                            , "   ('", BS8.pack optSyCFile, "')"
                                            ]

             _ | cab' == cab0 -> do
                     putStrLn "INFO: local and latest remote .cabal revision are already identical! Nothing to do."

             _ -> do
                     BS.writeFile bakfn cab0
                     putStrLn ("INFO: saved backup of original local file to " <> bakfn)

                     BS.writeFile optSyCFile cab'
                     BS8.putStrLn $ mconcat [ "local :  "
                                            , pkgn, "-", pkgv, "-r", BS8.pack (show $ xrev')
                                            , "   ('", BS8.pack optSyCFile, "')"
                                            ]


       ListCabal (ListCOptions {..}) -> do
           let pkgn = optLCPkgName

           vs <- runHConn (fetchVersions pkgn)

           unless optNoAnn $
             putStrLn $ concat [ "Found ", show (length vs), " package versions for "
                               , show pkgn, " ([U]npreferred, [D]eprecated):"
                               ]

           if optRevUrls then do
             forM_ vs $ \(v,_) -> do
                 let pid = pkgn <> "-" <> v
                 BS8.putStrLn $ mconcat [ " - https://hackage.haskell.org/package/", pid, "/revisions/" ]
           else do
             forM_ vs $ \(v,unp) -> do
                 let status = case unp of
                         _ | optNoAnn -> ""
                         Normal      -> "    "
                         Deprecated  -> "[D] "
                         UnPreferred -> "[U] "
                 BS8.putStrLn $ status <> pkgn <> "-" <> v
           return ()

       PushCabal (PushCOptions {..}) -> do
           (username,password) <- maybe (fail "missing Hackage credentials") return =<< getHackageCreds
           putStrLn $ "Using Hackage credentials for username " ++ show username

           forM_ optPsCFiles $ \fn -> do
               (pkgn, pkgv, xrev0) <- pkgDescToPkgIdXrev <$> C.readGenericPackageDescription C.deafening fn
               let xrev = applyWhen optPsCIncrRev (+1) xrev0

               putStrLn $ concat [ "Pushing ", show fn
                                 , " (", BS8.unpack pkgn, "-", BS8.unpack pkgv, "~", show xrev, ")"
                                 , if not optPsCPublish then " [review-mode]" else "", " ..."
                                 ]

               rawcab <- applyWhen optPsCIncrRev (cabalEditXRev xrev) <$> BS.readFile fn
               (dt, tmp) <- timeIt $ runHConn $
                 hackagePostCabal (username, password) (pkgn, pkgv) rawcab $
                   if optPsCPublish then WetRun else DryRun

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
           old <- BS.readFile optCROrig
           new <- BS.readFile optCRNew

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
                   forM_ changes $ \(Change _ what old' new') -> do
                       putStrLn $ "what: " ++ what
                       putStrLn $ " old: " ++ old'
                       putStrLn $ " new: " ++ new'

           return ()

       IndexShaSum opts -> IndexShaSum.run opts

       AddBound ab@AddBoundOptions{ optABFiles } -> do
         -- Run add-bound for all given cabal files, skipping to next on error.
         results <- forM optABFiles $ \fp -> do
           runExceptT (addBound (fp <$ ab)) >>= \case
             Left err -> False <$ log err
             Right () -> return True
         -- If add-bound failed for one cabal file, report failure.
         unless (and results) $ exitFailure

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

    getNetrcContents :: IO (Maybe ByteString)
    getNetrcContents = do
        mhome <- lookupEnv "HOME"
        case mhome of
            Nothing -> return Nothing
            Just "" -> return Nothing
            Just ho -> do
                let fnGpg = ho ++ "/.netrc.gpg"
                let fn = ho ++ "/.netrc"
                gpgExists <- doesFileExist fnGpg
                if gpgExists
                then readGpg fnGpg
                else readPlain fn
      where
        readGpg fn = do
            (ec, out, err) <- readProcessWithExitCode "gpg" ["--decrypt", fn] ""
            case ec of
                ExitSuccess   -> return (Just out)
                ExitFailure _ -> BS.putStr err >> return Nothing

        readPlain fn = do
            ret <- tryIOError (BS.readFile fn)
            case ret of
                Left e | isDoesNotExistError e -> return Nothing
                       | otherwise             -> ioError e
                Right b -> return $! Just b

    getHackageCreds :: IO (Maybe (ByteString,ByteString))
    getHackageCreds = do
        getNetrcContents >>= \case
            Nothing -> pure Nothing
            Just contents -> case parseNetRc "netrc" contents of
                Left _ -> fail "Invalid ${HOME}/.netrc(.gpg) found"
                Right NetRc {..} ->
                    evaluate $ (\NetRcHost{..} -> (nrhLogin,nrhPassword))
                               <$> listToMaybe (filter ((== optHost) . nrhName) nrHosts)


    pkgDescToPkgIdXrev pdesc = force (BS8.pack pkgn, BS8.pack $ showVersion pkgv, read xrev :: PkgRev)
      where
        C.PackageIdentifier (C.unPackageName -> pkgn) pkgv = C.package . C.packageDescription $ pdesc
        xrev = fromMaybe "0" . lookup "x-revision" . C.customFieldsPD . C.packageDescription $ pdesc

    incrXrev :: ByteString -> ByteString
    incrXrev cabdata0 = cabalEditXRev (xrev0+1) cabdata0
      where
        pdesc0 = parseGenericPackageDescription' cabdata0
        (_,_,xrev0) = pkgDescToPkgIdXrev pdesc0

parseGenericPackageDescription' :: ByteString -> LC.GenericPackageDescription
parseGenericPackageDescription' bs =
    case snd $ C.runParseResult $ C.parseGenericPackageDescription bs of
        Left (_, es) -> error $ List.intercalate "\n" $ map (C.showPError "<.cabal>") $ toList es
        Right x      -> x

extractRange :: LC.GenericPackageDescription -> C.PackageName -> C.VersionRange
extractRange gpd pkgName =
    List.foldl' C.intersectVersionRanges C.anyVersion vss
  where
    vss = gpd ^.. LC.condLibrary . _Just . condTreeDataL . LC.targetBuildDepends . traverse . to ext . _Just
    ext (C.Dependency pkgName' vr _)
       | pkgName == pkgName' = Just vr
       | otherwise           = Nothing

condTreeDataL :: Functor f => (a -> f a) -> C.CondTree v c a -> f (C.CondTree v c a)
condTreeDataL f (C.CondNode x c cs) = f x <&> \y -> C.CondNode y c cs


-- | Try to add the given bound to the given cabal file.
--
-- Non-fatal errors (like parse errors) are reported in the Except monad.
--
addBound :: AddBoundOptions' FilePath -> ExceptT String IO ()
addBound AddBoundOptions{ optABPackageName, optABVersionRange, optForce, optABMessage, optABFiles = fp } = do

  old <- liftIO $ BS.readFile fp

  -- idea is simple:
  -- - .cabal is line oriented file
  -- - find "library" section start
  -- - bonus: look of an indentation used from the next field/section there
  -- - insert data into a bytestring "manually"
  fs <- either (\ err -> throwError $ unwords ["Parsing", fp, "failed:", show err]) return $
      C.readFields old
  (lin, indent) <- maybe
      (throwError $ "Cannot find library section in " ++ fp)
      return
      (findLibrarySection fs)

  let msgLines  = map ("-- " ++) optABMessage
      bdLine    = "build-depends: " ++ C.prettyShow optABPackageName ++ " " ++ C.prettyShow optABVersionRange
      midLines  = [ BS8.pack $ replicate indent ' ' ++ l
                  | l <- msgLines ++ [bdLine]
                  ] ++ [""] -- also add an empty line separator
      (preLines, postLines) = splitAt lin $ BS8.lines old
      new = BS8.unlines (preLines ++ midLines ++ postLines)

  -- interpretation of version ranges
  let oldGpd = parseGenericPackageDescription' old
      newGpd = parseGenericPackageDescription' new

      oldRange = extractRange oldGpd optABPackageName
      newRange = extractRange newGpd optABPackageName

      oldRange' = C.intersectVersionRanges oldRange optABVersionRange

      -- Canonical forms (semantics)
      oldSem  = C.toVersionIntervals oldRange   -- existing range
      oldSem' = C.toVersionIntervals oldRange'  -- range after adding the bound (theory)
      newSem  = C.toVersionIntervals newRange   -- range after adding the bound (practice)

  -- Necessity check: does the addition of the bound change the semantics?
  -- if not, it can be skipped.

  if not optForce && oldSem' == oldSem then do

    log $ concat [ "Skipping ", fp, ": bound already subsumed by existing constraints (use --force to add nevertheless)." ]

  else do
    -- sanity check: did the addition have the intended outcome?
    unless (newSem == oldSem') $
      (if optForce then log . ("Ignoring check: " ++) else throwError . ("Edit failed, " ++)) $
         unwords
           [ "version ranges don't match: "
           , C.prettyShow oldRange
           , "&&"
           , C.prettyShow optABVersionRange
           , "=/="
           , C.prettyShow newRange
           ]

    -- write new version
    log $ unwords [ "Adding bound to", fp ]
    liftIO $ BS.writeFile fp new

-- | Print line to 'stderr'.
log :: MonadIO m => String -> m ()
log = liftIO . hPutStrLn stderr

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
