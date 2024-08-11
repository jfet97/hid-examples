{-# LANGUAGE StandaloneDeriving #-} -- needed for 'deriving instance Read ...'
{-# LANGUAGE OverloadedStrings #-}

import Radar
    ( orientMany,
      rotateMany,
      rotateManySteps,
      Direction(..),
      Turn(..) )

import System.Environment (getArgs)
import Fmt

deriving instance Read Direction
deriving instance Read Turn

-- | 'Buildable' instances for 'Direction' and 'Turn'.
-- the build method has to return a 'Builder' value, that's why we use OverloadedStrings
instance Buildable Direction where
  build North = "N"
  build East = "E"
  build South = "S"
  build West = "W"

instance Buildable Turn where
  build TNone = "--"
  build TLeft = "<-"
  build TRight = "->"
  build TAround = "||"

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname = do
  f <- readFile fname
  let turns = map read $ lines f
      finalDir = rotateMany dir turns
      dirs = rotateManySteps dir turns
  fmtLn $ "Final direction: "+||finalDir||+""
  fmt $ nameF "Intermediate directions" (unwordsF dirs)

-- unwordsF takes an `f a`, where f is some foldable wrapper, while a must be an instance of Buildable
-- and applies build from Buildable to every element

orientFromFile :: FilePath -> IO ()
orientFromFile fname = do
  f <- readFile fname
  let dirs = map read $ lines f
  fmt $ nameF "All turns" (unwordsF $ orientMany dirs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r", fname, dir] -> rotateFromFile (read dir) fname
    ["-o", fname] -> orientFromFile fname
    _ -> putStrLn $ "Usage: locator -o filename\n" ++
                    "       locator -r filename direction"
