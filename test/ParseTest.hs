{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_)
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

          incrParser = do
            let showT = T.pack . show
                render (pos, Nothing) = "END - " <> showT pos
                render (pos, Just (k, v)) =
                    k <> ": " <> T.pack (show v) <> " - " <> showT pos
                consumeEntries = await >>= \m'r -> forM_ m'r $ \r -> case r of
                  (_, Nothing) -> yield $ render r
                  (_, Just{}) -> do
                    yield $ render r
                    consumeEntries
            -- Get first entry, put it back on stream for reading.
            m'first <- C.conduitParser gcStatsIncrFirstEntry .| C.head
            case m'first of
              Nothing -> pure ()
              Just firstEntry -> yield $ render firstEntry
            C.conduitParser gcStatsIncr .| consumeEntries

          act = runConduitRes $ C.sourceFile gcStats
            .| C.decodeUtf8
            .| incrParser
            .| C.unlines
            .| C.encodeUtf8
            .| C.sinkFile outFile

      in goldenVsFile (takeBaseName gcStats <.> "incremental")
         goldenFile outFile act
