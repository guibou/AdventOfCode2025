module Day07 where

import Utils
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map

fileContent :: (V2 Int, Set (V2 Int), Int)
fileContent = parseContent $(getFile)

parseContent t = do
  let l = Text.lines t
  let splitters = Set.fromList $ do
         (line, lineNo) <- zip l [0..]
         (col, colNo) <- zip (Text.unpack line) [0..]
         guard $ col == '^'
         pure $ V2 lineNo colNo
  let Just startCol = Text.findIndex (=='S') (head l)
  (V2 0 startCol, splitters, length l)
  

-- * Generics

-- * FIRST problem
day :: (V2 Int, Set (V2 Int), Int) -> Int
day (V2 startLine startCol, splitters, height) = snd $ go (mempty, 0) [startCol] (startLine + 1)
  where
    go (tachyons, splitCount) currentBeamColumns currentLine
        | currentLine == height = (tachyons, splitCount)
        | otherwise = do
            let 
              lineBeams = Set.fromList $ map (V2 currentLine) currentBeamColumns 
              onSplitters = lineBeams `Set.intersection` splitters
              outSplitters = lineBeams `Set.difference` splitters
              generated = foldMap (\(V2 l c) -> Set.fromList [V2 l (c + 1), V2 l (c - 1)]) onSplitters
              thisLine = outSplitters <> generated
            go (tachyons <> thisLine, splitCount + Set.size onSplitters) (map (\(V2 _ c) -> c) $ Set.toList thisLine) (currentLine + 1)

-- * SECOND problem
day' :: (V2 Int, Set (V2 Int), Int) -> Int
day' (V2 startLine startCol, splitters, height) = go [(startCol, 1)] (startLine + 1)
  where
    go :: [(Int, Int)] -> Int -> Int
    go currentBeamColumnsAndTimelines currentLine
        | currentLine == height = sum (map snd currentBeamColumnsAndTimelines)
        | otherwise = do
            let 
              foo = do
                 (beam, timelines) <- map (\(col, timelines) -> (V2 currentLine col, timelines)) currentBeamColumnsAndTimelines
                 if beam `Set.member` splitters
                 then 
                    pure $ Right (beam, timelines)
                 else
                    pure $ Left (beam, timelines)
              (outSplitters, onSplitters) = partitionEithers foo
              generated = foldMap (\(V2 l c, timeline) -> [(V2 l (c + 1), timeline), (V2 l (c - 1), timeline)]) $ onSplitters
              newLine = map (\(V2 _ c, timeline) -> (c, timeline)) $ outSplitters <> generated
              newLine' = Map.toList $ Map.fromListWith (+) newLine
            go newLine' (currentLine + 1)


ex :: (V2 Int, Set (V2 Int), Int)
ex = parseContent """
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............

"""

-- started at Sun Dec  7 03:47:37 PM +04 2025
-- first at 16:08
-- second at 16:26
