{-# LANGUAGE TupleSections #-}
module Main (main) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Distribution.Server.Util.CabalRevisions (diffCabalRevisions', Change (..))
import Distribution.Simple.Utils (toUTF8BS)
import System.FilePath ((</>), (-<.>))
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsStringDiff)

main :: IO ()
main = defaultMain $ testGroup "Fixtures"
    -- basic sanity tests
    [ golden "tree-diff" 0 1
    , golden "deepseq"   0 1

    -- adding a new conditional section with restricted bounds
    -- TODO: this is not allowed
    , golden "SVGFonts" 0 2
    ]

golden :: String -> Int -> Int -> TestTree
golden name mi ma = case pairs [mi .. ma] of
    []       -> golden' name mi ma
    [(x, y)] -> golden' name x y
    ps       -> testGroup name
        [ golden' name x y
        | (x, y) <- ps
        ]

golden' :: String -> Int -> Int -> TestTree
golden' name mi ma = goldenVsStringDiff name' diff gold $ do
    o <- BS.readFile orig
    e <- BS.readFile edit
    return $ LBS.fromStrict $ toUTF8BS $ unlines $ case diffCabalRevisions' False o e of
        Left err      -> [ "ERROR", err ]
        Right changes -> "OK" : concatMap showChange changes
  where
    name' = unwords [ name, show mi, "->", show ma ]
    orig = "fixtures" </> name -<.> (show mi ++ ".cabal")
    edit = "fixtures" </> name -<.> (show ma ++ ".cabal")
    gold = "fixtures" </> name -<.> (show mi ++ "." ++ show ma ++ ".diff")

    diff ref new = ["diff", "-u", ref, new]

    showChange (Change sev what from to) =
        [ show sev
        , what
        , "- " ++ from
        , "+ " ++ to
        ]

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs
