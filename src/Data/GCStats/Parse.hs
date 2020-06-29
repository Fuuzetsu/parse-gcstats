{-# LANGUAGE ApplicativeDo #-}
module Data.GCStats.Parse
  ( gcStatsParser
  ) where

import Data.String
import qualified Data.Attoparsec.Text as P
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text
import qualified Data.Text as T
import GHC.Stats

-- | Parse a map of keys to values. All values are assumed to be
-- 'Double' though many are actually integers.
gcStatsParser :: P.Parser (HashMap Text Double)
gcStatsParser = do
  -- Skip first line which contains the command-line executed.
  P.takeTill P.isEndOfLine <* P.endOfLine
  _ <- P.skipSpace *> P.char '['
  entries <- entry `P.sepBy` (P.skipSpace <* P.char ',')
  -- We've parsed all the entries there were, for completion and to
  -- ensure nothing went wrong, expect the terminating bracket.
  _ <- P.skipSpace *> P.char ']'
  pure $! HashMap.fromList entries
  where
    key :: P.Parser Text
    key = P.char '"' *> P.takeTill (== '"') <* P.char '"'

    val :: P.Parser Double
    val = P.char '"' *> P.double <* P.char '"'

    entry :: P.Parser (Text, Double)
    entry = do
      _ <- P.char '('
      k <- key
      _ <- fromString ", "
      v <- val
      _ <- P.char ')'
      pure (k, v)
