module Day03 where

import Data.Text qualified as Text
import Utils

fileContent = parseContent $(getFile)

parseContent = Text.lines

-- * Generics

findJoltages :: Text -> [Int]
findJoltages input = do
  firstJolt <- ['9', '8' .. '1']
  let (_before, Text.drop 1 -> after) = Text.break (== firstJolt) input
  secondJolt <- ['9', '8' .. '0']
  guard $ secondJolt `Text.elem` after
  pure (read $ (firstJolt : secondJolt : []))

-- * FIRST problem

day banks = sum $ map (head . findJoltages) banks

-- * SECOND problem

combination _ 0 = [[]]
combination [] _ = []
combination (x : l) n = do
  ((x :) <$> combination l (n - 1)) <> combination l n

findJoltages' :: Text -> Int
findJoltages' text = read $ Text.unpack $ go 12 text
  where
    go 0 _ = ""
    go n t
      | Text.length t == n = t
      | otherwise = reduce '9' n t

    reduce '0' _ _ = error "I don't know what to do here"
    reduce c n t = do
      let (_before, after) = Text.break (== c) t
      if Text.length after < n
        then
          reduce (pred c) n t
        else
          Text.cons c (go (n - 1) (Text.drop 1 after))

day' :: [Text] -> Int
day' banks = sum $ map findJoltages' banks

ex =
  parseContent
    """
    987654321111111
    811111111111119
    234234234234278
    818181911112111

    """

-- started at Wed Dec  3 01:33:17 PM +04 2025
-- first at 1:42
--
-- too low: 148318887843054
-- second star: 15:02 (I had a lunch and swimming pool break ;)
