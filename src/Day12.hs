module Day12 where

import Utils
import qualified Data.Text as Text
import qualified Data.Set as Set
import Control.Applicative.Combinators (sepBy)
import Text.Megaparsec (label, sepBy1)

fileContent = parseContent $(getFile)

parseContent t = do
  let items = Text.splitOn "\n\n" t
      shapes = init items
      regions = last items
  (map parseShape shapes, unsafeParse parseRegions regions)

parseRegions = some (parseRegion <* "\n")

parseRegion = do
  width <- parseNumber
  "x"
  length <- parseNumber
  ": "
  quantities <- some parseNumber

  pure Region{..}

data Region = Region {
  width :: Int,
  length :: Int,
  quantities :: [Int]
} deriving (Show)

parseShape t = Set.fromList $ do
  (lineNo, l) <- zip [0..] (drop 1 $ Text.lines t)
  (colNo, c) <- zip [0..] (Text.unpack l)
  guard $ c == '#'
  pure $ V2 lineNo colNo

-- * Generics

-- * FIRST problem
day = undefined

-- * SECOND problem
day' = undefined

ex = parseContent """
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2

"""

-- started at Sat Dec 13 02:06:52 PM +04 2025
