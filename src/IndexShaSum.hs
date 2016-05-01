{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  IndexShaSum
-- Copyright   :  Herbert Valerio Riedel
-- License     :  GPL-3
--
module IndexShaSum (run, IndexShaSumOptions(..)) where

import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Monad
import qualified Data.Aeson             as J
import qualified Data.Aeson.Types       as J
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Short  as BSS
import qualified Data.HashMap.Strict    as HM
import           Data.Maybe
import           Data.Monoid
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.String
import           Data.Text.Encoding     as T
import           System.FilePath

data IndexShaSumOptions = IndexShaSumOptions
    { optFlatStyle   :: Bool
    , optISSIndexTar :: FilePath
    , optBaseDir     :: Maybe FilePath
    } deriving Show

type SrcTarName   = BSS.ShortByteString -- with .tar.gz suffix
type SrcTarSha256 = BSS.ShortByteString -- base16

run :: IndexShaSumOptions -> IO ()
run (IndexShaSumOptions {..}) = do
    idx <- readTarEntries optISSIndexTar
    forM_ (collect idx) (uncurry printSumLine)
  where
    printSumLine fn sh256 = BS.putStr line
      where
        line = mconcat [BSS.fromShort (fixupSum sh256), "  " , bdirpfx, BSS.fromShort fn', "\n"]
        bdirpfx = maybe "" fromString optBaseDir
        fn' = if optFlatStyle then fn else unFlat fn

    -- | Missing checksums are denoted by a 0-checksum
    fixupSum x
       | BSS.null x  = BSS.toShort (BS.replicate 64 48)
       | otherwise   = x

    collect :: [Tar.Entry] -> [(SrcTarName,SrcTarSha256)]
    collect = go mempty mempty

    go :: Set SrcTarName -> Set SrcTarName -> [Tar.Entry] -> [(SrcTarName,SrcTarSha256)]
    go !seen1 !seen2 []
      | missingCabs <- Set.difference seen1 seen2
      , not (Set.null missingCabs) = error "missing .cabal file(s)"
      | otherwise -- append files with missing checksum
      = [ (missingSum, "") | missingSum <- Set.toList (Set.difference seen2 seen1) ]
    go !seen1 !seen2 (e:es)
      | takeExtension fn == ".cabal"
      , [pn,pv,_cn] <- splitDirectories fn
      = let fn' = fromString (pn ++ "-" ++ pv ++ ".tar.gz")
        in go seen1 (Set.insert fn' seen2) es
      | takeFileName fn == "package.json"
      , Tar.NormalFile bs _sz <- Tar.entryContent e
      = let (fn',cksum) = fromMaybe undefined (decodePkgJsonFile bs)
        in if Set.member fn' seen1
           then go seen1 seen2 es
           else ((fn',cksum) : go (Set.insert fn' seen1) seen2 es)
      | otherwise = go seen1 seen2 es
      where
        fn = Tar.entryPath e

-- | Convert to non-flat layout (i.e. @<name>/<ver>/<name>-<ver>.tar.gz@)
unFlat :: SrcTarName -> SrcTarName
unFlat fn0 = BSS.toShort $ mconcat [pn <> "/" <> pv <> "/" <> fn0']
  where
    fn0' = BSS.fromShort fn0

    Just base = stripSuffixBS ".tar.gz" fn0'

    (pn_, pv) = BS.spanEnd (\c -> (c >= 0x30 && c <= 0x3a) || c == 0x2e) base
    Just (pn, 0x2d) = BS.unsnoc pn_

-- | Read tarball lazily (and possibly decompress)
readTarEntries :: FilePath -> IO [Tar.Entry]
readTarEntries idxtar = do
    es <- case takeExtension idxtar of
            ".gz"  -> Tar.read . GZip.decompress <$> BSL.readFile idxtar
            ".tar" -> Tar.read                   <$> BSL.readFile idxtar
            ext    -> error ("unknown extension " ++ show ext)

    return (Tar.foldEntries (:) [] (\err -> error ("readTarEntries " ++ show err)) es)

-- | Decode and extract source-tarball filename and sha256 checksum from TUF @package.json@
decodePkgJsonFile :: BSL.ByteString -> Maybe (SrcTarName, SrcTarSha256)
decodePkgJsonFile bs = do
    metainfo <- J.decode' bs
    [(fn,s256)] <- packagejson2sha metainfo

    return $! strictPair (BSS.toShort $ normaliseFn fn) (BSS.toShort s256)
  where
    normaliseFn fn = fromMaybe fn $ stripPrefixBS "<repo>/package/" fn

    packagejson2sha :: J.Value -> Maybe [(BS.ByteString, BS.ByteString)]
    packagejson2sha = J.parseMaybe go1
      where
        go1 = J.withObject "PackageJson" $ \o -> do
            signed   <- o      J..: "signed"
            targets  <- signed J..: "targets"
            J.withObject "PackageJson.signed.targets" go2 targets

        go2 m = forM (HM.toList m) $ \(k,v) -> do
            J.withObject ".targets{}" (go3 k) v

        go3 k o = do
            hashes <- o J..: "hashes"
            sh256 <- hashes J..: "sha256"
            return (T.encodeUtf8 k, T.encodeUtf8 sh256)

strictPair :: a -> b -> (a,b)
strictPair !a !b = (a,b)

stripPrefixBS :: ByteString -> ByteString -> Maybe ByteString
stripPrefixBS pfx b
  | BS.isPrefixOf pfx b = Just $ BS.drop (BS.length pfx) b
  | otherwise           = Nothing


stripSuffixBS :: ByteString -> ByteString -> Maybe ByteString
stripSuffixBS sfx b
  | BS.isSuffixOf sfx b = Just $ BS.take (BS.length b - BS.length sfx) b
  | otherwise           = Nothing
