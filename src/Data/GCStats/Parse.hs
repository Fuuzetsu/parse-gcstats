module Data.GCStats.Parse
  ( gcStatsParser
  , gcStatsIncr
  , gcStatsIncrSkipEnd
  , gcStatsIncrFirstEntry
  ) where

import Control.Applicative (optional, (<|>))
import Control.Monad (void)
import qualified Data.Attoparsec.Text as P
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String
import Data.Text
import qualified Data.Text as T
import GHC.Stats

-- | Parse a map of keys to values. All values are assumed to be
-- 'Double' though many are actually integers.
gcStatsParser :: P.Parser (HashMap Text Double)
gcStatsParser = do
  m'firstEntry <- gcStatsIncrFirstEntry
  entries <- case m'firstEntry of
    -- No point trying to parse more if we can't even get first entry.
    Nothing -> pure mempty
    Just firstEntry -> (:) firstEntry <$> P.many' nextEntry
  -- We've parsed all the entries there were, for completion and to
  -- ensure nothing went wrong, expect the terminating bracket.
  gcStatsIncrSkipEnd
  pure $! HashMap.fromList entries

-- | Skip start of file which contains command line use and yield
-- first entry, if there is one. You should run this before
-- 'gcStatsIncr'.
gcStatsIncrFirstEntry :: P.Parser (Maybe (Text, Double))
gcStatsIncrFirstEntry = do
  P.skipWhile (not . P.isEndOfLine)
  P.skipSpace
  void $ P.char '['
  optional entry

-- | Check if we have a closing brace, indicating end of entries.
--
-- Does not check for EOF.
gcStatsIncrSkipEnd :: P.Parser ()
gcStatsIncrSkipEnd = P.skipSpace <* P.char ']'

-- | Parser that returns entries as they come in, effectively
-- tokenising the input. In interest of streaming, does no input
-- validation on whether the sequence of tokens it produces actually
-- makes sense.
--
-- Whatever way you're parsing, you ought to first do one run of
-- 'gcStatsIncrFirstEntry' after which you can invoke 'gcStatsIncr'
-- until it no longer matches. This function returns @Nothing@ if end
-- of entries is seen.
gcStatsIncr :: P.Parser (Maybe (Text, Double))
gcStatsIncr = (Just <$> nextEntry) <|> (Nothing <$ gcStatsIncrSkipEnd)

entry :: P.Parser (Text, Double)
entry = do
  _ <- P.char '('
  k <- key
  _ <- fromString ", "
  v <- val
  _ <- P.char ')'
  pure (k, v)
  where
    key :: P.Parser Text
    key = P.char '"' *> P.takeTill (== '"') <* P.char '"'

    val :: P.Parser Double
    val = P.char '"' *> P.double <* P.char '"'

-- | Some non-first entry.
nextEntry :: P.Parser (Text, Double)
nextEntry = do
  -- Non-first entry, has to have separator in front.
  P.skipSpace *> P.char ',' *> P.skipSpace
  -- We should be at an entry now, output it.
  entry
