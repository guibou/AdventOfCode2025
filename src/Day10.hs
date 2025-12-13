module Day10 where

import Utils hiding (many)
import Text.Megaparsec
import qualified Data.Set as Set
import qualified Data.Vector as Vector

data Machine = Machine {
  lights :: Vector Bool,
  buttons :: [Button],
  jolts :: [Int]
  }
  deriving (Show)

data Button = Button (Set Int)
  deriving (Show)

fileContent = parseContent $(getFile)

parseContent = unsafeParse (many (parseMachine <* "\n"))

parseMachine = do
  lights <- parseLights
  " "
  buttons <- many (parseButton <* " ")
  jolts <- parseJolts

  pure Machine{..}

parseJolts = "{" *> (parseNumber `sepBy` ",") <* "}"

parseLights = "[" *> (Vector.fromList <$> (many $ choice ["." $> False, "#" $> True])) <* "]"

parseButton = Button . Set.fromList <$> ("(" *> (parseNumber `sepBy` ",") <* ")")

-- * Generics
solveMachine :: Machine -> Int
solveMachine machine = go [Vector.replicate (length machine.lights) False] 0 
  where
    go currentState !steps = do
       let newStates = Set.fromList $ do
                  state <- currentState
                  Button button <- machine.buttons
                  pure $ applyButton button state
       if machine.lights `Set.member` newStates
       then steps + 1
       else go (Set.toList newStates) (steps + 1)

applyButton :: Set Int -> Vector Bool -> Vector Bool
applyButton s state = Vector.imap (\i v -> if i `Set.member` s then not v else v) state

-- * FIRST problem
day = sum . map solveMachine

-- * SECOND problem
day' = undefined

ex = parseContent """
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

"""

-- started at Sat Dec 13 02:05:18 PM +04 2025
