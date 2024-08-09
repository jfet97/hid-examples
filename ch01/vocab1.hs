import Data.Char
import Data.List (group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment -- command line arguments

main :: IO ()
main = do
  [fname] <- getArgs -- doesn't include the program name, we suppose just one argument
  text <- TIO.readFile fname
  let ws = map head $ group $ sort $ map T.toCaseFold $ filter (not . T.null)
           $ map (T.dropAround $ not . isLetter) $ T.words text
  TIO.putStrLn $ T.unwords ws
  print $ length ws

-- dropAround is like trim, but with a custom predicate
-- toCaseFold is like toLower, but works for more languages, it's more resilient, faster, respect Unicode
