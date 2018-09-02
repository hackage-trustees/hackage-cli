{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright   :  Herbert Valerio Riedel
-- License     :  GPL-3
module CabalEdit
    ( cabalEditXRev
    , cabalSplitAtField
    , PkgRev
    ) where

import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Char8                  as BS8
import           Data.Semigroup
import qualified Distribution.Parsec.Common             as C
import qualified Distribution.Parsec.Field              as C
import qualified Distribution.Parsec.Parser             as C

type PkgRev  = Word

-- | Insert or replace existing "x-revision" line
--
-- __NOTE__: This uses 'cabalSplitAtField' and therefore currently
-- supports only simplified (i.e. without use of @{@ @}@ layout
-- tokens) Cabal file grammar
cabalEditXRev :: PkgRev -> ByteString -> ByteString
cabalEditXRev xrev oldcab = pre1 <> mid1 <> xrevLine <> post1
  where
    (pre0,_,post0)    = either (error . show) id $ cabalSplitAtField "x-revision" oldcab
    (pre1,mid1,post1) = either (error . show) id $ cabalSplitAtField "version" (pre0 <> post0)

    ls = BS8.lines oldcab

    xrevLine = "x-revision: " <> BS8.pack (show xrev) <> if isCRLF then "\r\n" else "\n"

    -- simple heuristic
    isCRLF = case ls of
        []     -> False
        ("":_) -> False
        (l1:_) -> BS8.last l1 == '\r'


-- | Split a cabal file into three fragments: before the (first
-- occurence of a field), the field, and the rest after the field.
--
-- This should, in spirit, follow the invariant (assuming there are no parsing failures):
--
-- > cabalSplitAtField someField (pre <> mid <> post) == Right (pre, mid, post)
--
-- If field not found, the middle and trailing fragment will be empty 'ByteString's
--
-- This operation is quite universal; it can be used to remove fields,
-- or insert content before a field or after a field etc
--
-- __NOTE__: This doesn't properly handle layout-mode (i.e. as controlled by the @{@ and @}@ tokens) yet
cabalSplitAtField :: ByteString -- ^ fieldname
                  -> ByteString -- ^ @.cabal@ description
                  -> Either String (ByteString,ByteString,ByteString) -- ^ pre, mid, post
cabalSplitAtField fname cab0 = do
    fields <- either (Left . show) Right $ C.readFields cab0

    case [ (npos, vals) | C.Field (C.Name (C.Position npos _) n) vals <- fields, n == fname ] of
      [] -> pure (cab0, BS8.empty, BS8.empty)
      (npos, vals):_ -> case vals of
                          [] -> pure $! go (npos-1) 1

                          _ -> let C.FieldLine (C.Position pos2 _) _ = last vals
                               in  pure $! go (npos-1) (1+pos2-npos)

  where
    cab0lines = BS8.lines cab0

    -- TODO: detect '{'s in `mid` and fixup k to include the closing '}'
    --
    -- NB: it's not enough to simply look at the next field's line
    -- number, as we don't want 'mid' to include trailing comments
    -- which conventionally belong to the next fields; i.e. in case of
    -- something like
    --
    --   name: foo
    --   version: 1.2.3
    --   x-revision: {
    --   42
    --   }
    --   -- for reasons this must remain Custom instead of Simple
    --   build-type: Custm
    --   synopsis: ...
    --
    -- We want the middle of 'cabalSplitAtField "x-revision"' to stop
    -- right after the '}' and *not* include the comment
    --
    go j k = (BS8.unlines pre, BS8.unlines mid, BS8.unlines post)
      where
        (pre, midpost) = splitAt j cab0lines
        (mid,post)     = splitAt k midpost
