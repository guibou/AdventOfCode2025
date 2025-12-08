module Day08 where

import Control.Monad.Union qualified as Union
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Vector qualified as Vector
import Utils

fileContent = parseContent $(getFile)

distanceSquared :: V3 Int -> V3 Int -> Int
distanceSquared a b = do
  let c = b - a
  dot c c

parseContent :: Text -> Vector.Vector (V3 Int)
parseContent = Vector.fromList . (unsafeParse $ many (V3 <$> parseNumber <*> ("," *> parseNumber) <*> ("," *> parseNumber) <* "\n"))

-- * Generics

distanceMap junctions = do
  -- This is O(n^2), but that's acceptable with the current problem size
  -- Another solution would be to build a space partition (kdTree)
  (junctionA, iA) <- zip (Vector.toList junctions) [0 ..]
  (junctionB, iB) <- zip (drop (iA + 1) $ Vector.toList junctions) [(iA + 1) ..]

  pure ((iA, iB), distanceSquared junctionA junctionB)

connectNodes nodes (iA, iB) =
  Union.merge (\a b -> (min a b, (iA, iB))) (nodes Vector.! iA) (nodes Vector.! iB)

withDistanceMap junctions action = do
  let distances = map fst $ sortBy (comparing snd) $ distanceMap junctions
  Union.run $ do
    (Vector.fromList -> nodes) <- mapM Union.new [0 .. Vector.length junctions - 1]
    action distances nodes
-- * FIRST problem

day junctions nbConnections = do
  let circuits = Map.elems $ computeUnionMap junctions nbConnections
      sorted = take 3 $ reverse $ sort $ map length circuits
  product $ sorted

computeUnionMap junctions nbConnections = do
  withDistanceMap junctions $ \distances nodes -> do
        mapM_ (connectNodes nodes) $ take nbConnections distances

        res <-
          mapM
            ( \(nodeI, node) -> do
                (_, nodeLabel) <- Union.lookup node
                pure (nodeLabel, [nodeI])
            )
            (zip [0 :: Int ..] $ Vector.toList nodes)
        pure $ Map.fromListWith (<>) res

-- * SECOND problem

day' junctions = do
  let (iA, iB) = connectAll junctions
      (V3 x _ _, V3 x' _ _) = (junctions Vector.! iA, junctions Vector.! iB)
  x * x'

connectAll junctions = do
  withDistanceMap junctions $ \distances nodes -> do
    connections <- mapM (connectNodes nodes) distances
    pure (last $ catMaybes connections)

ex =
  parseContent
    """
    162,817,812
    57,618,57
    906,360,560
    592,479,940
    352,342,300
    466,668,158
    542,29,236
    431,825,988
    739,650,466
    52,470,668
    216,146,977
    819,987,18
    117,168,530
    805,96,715
    346,949,466
    970,615,88
    941,993,340
    862,61,35
    984,92,344
    425,690,689

    """

-- started at Mon Dec  8 09:00:50 AM +04 2025
-- first at 9:49 (with a huge break to build a correct union find library ;)
-- second at 9:58
