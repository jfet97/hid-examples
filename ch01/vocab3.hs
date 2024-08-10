{-# LANGUAGE OverloadedStrings #-}

-- String literals become values of type Text
-- so that we don't have to convert them using T.pack

-- not need to qualify Text as T.Text

import Control.Monad (when)
import Data.Char
import Data.List (group, sort, sortBy)
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import System.Environment

type Entry = (Text, Int)

type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws =
      map T.toCaseFold $
        filter (not . T.null) $
          map cleanWord $
            T.words t
    buildEntry xs@(x : _) = (x, length xs)
    buildEntry [] = error "unexpected"
    cleanWord = T.dropAround (not . isLetter)

allWords :: Vocabulary -> [Text]
allWords = map fst

-- (total number of words, number of unique words)
wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocab = (sum $ map snd vocab, length vocab)

-- Down wraps a value and reverse the order of Ord instance
wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

-- unlinesF combines a list of Text values into a single Builder
-- nameF is a formatter that adds a title to a block of text
allWordsReport :: Vocabulary -> Text
allWordsReport vocab =
  fmt $ nameF "All words" $ unlinesF (allWords vocab)

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab = fmt report
  where
    (total, unique) = wordsCount vocab
    report =
      "Total number of words: "
        +| total
        |+ "\nNumber of unique words: "
        +| unique
        |+ "\n"

-- without builder
-- wordsCountReport' :: Vocabulary -> Text
-- wordsCountReport' vocab = T.unlines [part1, part2]
--   where
--     (total, unique) = wordsCount vocab
--     part1 =
--       T.append
--         (T.pack "Total number of words: ")
--         (T.pack $ show total)
--     part2 =
--       T.append
--         (T.pack "Number of unique words: ")
--         (T.pack $ show unique)

-- blockListF' formats list elements in the given way and presents them line by line
frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab num =
  fmt $
    nameF "Frequent words" $
      blockListF' "" fmtEntry reportData
  where
    -- exploit lazyness: wordsByFrequency won't compute all the list
    -- there is no need to pass num to wordsByFrequency as well
    reportData = take num $ wordsByFrequency vocab
    fmtEntry (t, n) = "" +| t |+ ": " +| n |+ ""

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname withAllWords n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  -- when withAllWords takes an IO action and executes it only if the condition is True
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab -- same as _ <- when ...
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", fname, num] ->
      processTextFile fname True (read num)
    [fname, num] ->
      processTextFile fname False (read num)
    _ -> putStrLn "Usage: vocab3 [-a] filename freq_words_num"
