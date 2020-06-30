{-# LANGUAGE BangPatterns #-}
module Data.GCStats.Parse
  ( gcStatsParser
  , gcStatsIncr
  , ParserState
  , initialParserState
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
gcStatsParser =
  let getEntries !m ps = do
        m'e <- gcStatsIncr ps
        case m'e of
          Nothing -> pure m
          Just ((k, v), ps') -> getEntries (HashMap.insert k v m) ps'
  in getEntries mempty initialParserState

data ParserState
  = AtStart
  | PastStart

initialParserState :: ParserState
initialParserState = AtStart

-- | Parser that returns entries as they come in, effectively
-- streaming the entries.
--
-- You have to pass subsequent 'ParserState' results to next
-- invocations. If function returns 'Nothing', parsing is finished.
gcStatsIncr :: ParserState -> P.Parser (Maybe ((Text, Double), ParserState))
gcStatsIncr ps = case ps of
  AtStart -> fmap nextSt gcStatsIncrFirstEntry
  PastStart ->
    let entryOrEnd = (Just <$> nextEntry) <|> (Nothing <$ gcStatsIncrSkipEnd)
    in fmap nextSt entryOrEnd
  where
    nextSt Nothing = Nothing
    nextSt (Just e) = Just (e, PastStart)

    -- Skip start of file which contains command line use and yield
    -- first entry, if there is one.
    gcStatsIncrFirstEntry :: P.Parser (Maybe (Text, Double))
    gcStatsIncrFirstEntry = do
      P.skipWhile (not . P.isEndOfLine)
      P.skipSpace
      void $ P.char '['
      optional entry

    -- Check if we have a closing brace, indicating end of entries.
    --
    -- Does not check for EOF.
    gcStatsIncrSkipEnd :: P.Parser ()
    gcStatsIncrSkipEnd = P.skipSpace <* P.char ']'


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
