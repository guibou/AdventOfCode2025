module Day01 where

import Utils
import qualified Data.Text as Text

fileContent :: [(Direction, Int)]
fileContent = parseContent $(getFile)

data Direction = L | R
  deriving (Show, Eq)

toDirection ('R':i) = (R, read i)
toDirection ('L':i) = (L, read i)

parseContent t = fmap (toDirection . Text.unpack) (Text.lines t)

-- * Generics

-- * FIRST problem
day l = snd $ foldl' (flip f) (50, 0) l
  where
    f (dir, x) (y, c) = let
      op = case dir of
        R -> (+)
        L -> (-)
      y' = (y `op` x) `mod` 100
      in (y', if y' == 0 then c + 1 else c)

day' l = snd $ go (50, 0) l
  where
    go (pos, count) [] = (pos, count)
    go acc ((_, 0):xs) = go acc xs
    go (pos, count) ((dir, dx):xs) = do
           let (dc, dx') = dx `divMod` 100
           let pos' = (pos + case dir of
                     L -> -dx'
                     R -> dx')
           go (pos' `mod` 100, dc + (if (pos' >= 100 || (pos' <= 0 && pos /= 0)) then count + 1 else count)) xs

ex :: [(Direction, Int)]
ex = parseContent """
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82

"""

-- started at Mon Dec  1 08:57:15 AM +04 2025
--
-- star2: 2270 is not the right answer
-- star2: 6580 is not the rigth answer
