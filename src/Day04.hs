module Day04 where

import Utils
import qualified Data.Text as Text
import qualified Data.Set as Set

fileContent :: Set (V2 Int)
fileContent = parseContent $(getFile)

parseContent t = do
  let
    ls = Text.lines t
    s = Set.fromList $ do
     (lineNo, l) <- zip [0..] ls
     (colNo, c) <- zip [0..] (Text.unpack l)

     guard $ c == '@'
     pure (V2 lineNo colNo)
  s
  
  

-- * Generics

-- * FIRST problem
day :: Set (V2 Int) -> Int
day = length . removeableRoll

removeableRoll s = do
  pos  <- Set.toList s

  let rollNeigbors = do
        dpos <- drop 1 $ connect8
        let pos' = pos + dpos
        guard $ pos' `Set.member` s
        pure pos

  guard $ length rollNeigbors < 4
  pure pos
  

-- * SECOND problem
day' s = Set.size s - Set.size finals
  where
    finals = go s
    go s = do
       let removable = removeableRoll s
           s' = s `Set.difference` (Set.fromList removable)
       if s' == s then s
       else go s'

ex :: Set (V2 Int)
ex = parseContent """
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.

"""

-- started at Thu Dec  4 11:03:48 AM +04 2025
-- first at 11:15. Second at 11:19
