module Main (main) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Distribution.Server.Util.CabalRevisions (diffCabalRevisions, Change (..))
import Distribution.Simple.Utils (toUTF8BS)
import System.FilePath ((</>), (-<.>))
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsStringDiff)

main :: IO ()
main = defaultMain $ testGroup "Fixtures"
    [ golden "tree-diff"
    ]

golden :: String -> TestTree
golden name = goldenVsStringDiff name diff gold $ do
    o <- BS.readFile orig
    e <- BS.readFile edit
    return $ LBS.fromStrict $ toUTF8BS $ unlines $ case diffCabalRevisions o e of
        Left err      -> [ "ERROR", err ]
        Right changes -> "OK" : concatMap showChange changes
  where
    orig = "fixtures" </> name -<.> "orig.cabal"
    edit = "fixtures" </> name -<.> "edit.cabal"
    gold = "fixtures" </> name -<.> "diff"

    diff ref new = ["diff", "-u", ref, new]

    showChange (Change sev what from to) =
        [ show sev
        , what
        , "- " ++ from
        , "+ " ++ to
        ]
