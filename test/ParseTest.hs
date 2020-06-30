{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_, join)
import Data.Attoparsec.Text (parseOnly)
import Data.Conduit
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Combinators as C
import Data.GCStats.Parse
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath (takeBaseName, (<.>), replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)

main :: IO ()
main = goldenTests >>= defaultMain

goldenTests :: IO TestTree
goldenTests = do
  gcStatsFiles <- findByExtension [".gcstats"] "."
  pure $ testGroup "gcStatsParser golden tests" $ mconcat
    [ parseTest <$> gcStatsFiles
    , parseTestIncr <$> gcStatsFiles
    ]
  where
    parseTest :: FilePath -> TestTree
    parseTest gcStats =
      let goldenFile = gcStats <.> "golden"
          outFile = replaceExtension goldenFile "out"
      in goldenVsFile (takeBaseName gcStats)
         goldenFile
         outFile
         (parseOnly gcStatsParser <$> T.readFile gcStats >>= writeFile outFile . show)

    -- Test incremental parsing with conduit
    parseTestIncr :: FilePath -> TestTree
    parseTestIncr gcStats =
      let goldenFile = gcStats <.> "incremental.golden"
          outFile = replaceExtension goldenFile "out"

          -- Return single entry an entry if it exists.
          entriesC ps = do
            m'e <- C.sinkParser (gcStatsIncr ps)
            forM_ m'e $ \(e, ps') -> do
              yield e
              entriesC ps'

          render :: (T.Text, Double) -> T.Text
          render (k, v) = k <> ": " <> T.pack (show v)

          act = runConduitRes $ C.sourceFile gcStats
            .| C.decodeUtf8
            .| entriesC initialParserState
            .| C.map render
            .| C.unlines
            .| C.encodeUtf8
            .| C.sinkFile outFile

      in goldenVsFile (takeBaseName gcStats <.> "incremental")
         goldenFile outFile act
