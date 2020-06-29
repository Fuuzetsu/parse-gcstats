module Main (main) where

import Data.Attoparsec.Text (parseOnly)
import Data.GCStats.Parse
import qualified Data.Text.IO as T
import System.FilePath (takeBaseName, (<.>), replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)

main :: IO ()
main = goldenTests >>= defaultMain

goldenTests :: IO TestTree
goldenTests = do
  gcStatsFiles <- findByExtension [".gcstats"] "."
  pure $ testGroup "gcStatsParser golden tests"
    (parseTest <$> gcStatsFiles)
  where
    parseTest :: FilePath -> TestTree
    parseTest gcStats =
      let goldenFile = gcStats <.> "golden"
          outFile = replaceExtension goldenFile "out"
      in goldenVsFile (takeBaseName gcStats)
         goldenFile
         outFile
         (parseOnly gcStatsParser <$> T.readFile gcStats >>= writeFile outFile . show)
