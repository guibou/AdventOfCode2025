module Day05 where

import Utils hiding (many)
import Text.Megaparsec
import Data.Range

fileContent = parseContent $(getFile)

parseContent = unsafeParse $  do
  ranges <- many (parseRange <* "\n")
  void "\n"
  ids <- many (parseNumber <* "\n")

  pure (ranges, ids :: [Int])

parseRange :: Parser (Range Int)
parseRange = do
  a <- parseNumber
  void "-"
  b <- parseNumber
  pure $ a +=+ b

-- * Generics

-- * FIRST problem
day (ranges, ids) = length (filter (inRanges ranges) ids)


-- * SECOND problem
rangeLength (SpanRange (Bound a _) (Bound b _)) = b - a + 1
rangeLength (SingletonRange _)  = 1
rangeLength e = error (show e)

day' (ranges, _) = sum $ map rangeLength $ mergeRanges ranges

ex = parseContent """
3-5
10-14
16-20
12-18

1
5
8
11
17
32

"""


-- started at Sat Dec  6 06:26:25 PM +04 2025
-- both at 18:46
