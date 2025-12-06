module Day06 where

import Utils
import qualified Data.Text as Text
import Data.List

fileContent = $(getFile)

parseContent :: Text -> ([[Int]], [Text])
parseContent t = do
  let
    l = Text.lines t
    cases = map Text.words l
    numbers = map (map $ read . Text.unpack) $ init cases
    operations = last cases

  (numbers, operations)

parseContent' :: Text -> ([[Int]], [Text])
parseContent' t = do
  let
    l = Text.split (=='\n') t
    values = map (map (Text.unpack)) $ groupir $ map (Text.pack) $ (transpose $ map (Text.unpack) $ init $ init l)
    operations = Text.words $ last $ Text.lines t

  (reverse $ map (map (read . Text.unpack . Text.strip . Text.pack)) values, reverse operations)

groupir l = go l []
  where
    go [] acc = [acc]
    go (x:xs) acc = if Text.null (Text.strip x)
                then acc:go xs []
                else go xs (x:acc)

-- * Generics

-- * FIRST problem
day t = do
  let
    (numbers, op) = parseContent t
    numbers' = transpose numbers
  sum (zipWith applyOp numbers' op)

applyOp numbers "*" = product numbers
applyOp numbers "+" = sum numbers

-- * SECOND problem
day' t = do
  let (numbers, op) = parseContent' t
  sum (zipWith applyOp numbers op)

ex :: Text
ex = """
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   + 

"""

-- started at Sat Dec  6 06:58:26 PM +04 2025
-- first at 18:04
-- 2237711 answer is too low
-- second at 19:26
