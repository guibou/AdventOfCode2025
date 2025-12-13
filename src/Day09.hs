module Day09 where

import Utils

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ many (parseCoord <* "\n")

parseCoord = V2 <$> parseNumber <*> ("," *> parseNumber)

-- * Generics

-- * FIRST problem
day coords = maximum $ do
  c0 <- coords
  c1 <- coords
  guard $ c1 /= c0
  let V2 dx dy = c1 - c0
  pure $ abs (dx + 1) * abs (dy + 1)

-- * SECOND problem
day' = undefined

ex = parseContent """
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3

"""

-- started at Sat Dec 13 01:57:45 PM +04 2025
