module Exercises
  ( change,
    firstThenApply,
    powers,
    meaningfulLineCount,
  )
where

import Control.Exception (handle)
import Data.Char (isSpace)
import Data.List (find, isPrefixOf)
import Data.Map qualified as Map
import Data.Text (pack, replace, unpack)
import GHC.Data.ShortText (ShortText (contents))
import GHC.IO.FD (openFile)

change :: Integer -> Either String (Map.Map Integer Integer)
change amount
  | amount < 0 = Left "amount cannot be negative"
  | otherwise = Right $ changeHelper [25, 10, 5, 1] amount Map.empty
  where
    changeHelper [] remaining counts = counts
    changeHelper (d : ds) remaining counts =
      changeHelper ds newRemaining newCounts
      where
        (count, newRemaining) = remaining `divMod` d
        newCounts = Map.insert d count counts

firstThenApply :: [a] -> (a -> Bool) -> (a -> b) -> Maybe b
firstThenApply xs p f = f <$> find p xs

powers :: (Integral a) => a -> [a]
powers base = map (base ^) [0 ..]

meaningfulLineCount :: FilePath -> IO Int
meaningfulLineCount path = do
  contents <- readFile path
  let linesOfFile = lines contents
      validLines = filter isValidLine linesOfFile
  return (length validLines)

isValidLine :: String -> Bool
isValidLine line =
  not (null trimmedLine)
  && not (all isSpace trimmedLine)
  && not (startsWithHash trimmedLine)
  where
    trimmedLine = dropWhile isSpace line

startsWithHash :: String -> Bool
startsWithHash [] = False
startsWithHash (x : _) = x == '#'

-- Write your shape data type here

-- Write your binary search tree algebraic type here
