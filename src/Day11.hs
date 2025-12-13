module Day11 where

import Control.Applicative.Combinators (count)
import Data.Map.Strict qualified as Map
import Text.Megaparsec (oneOf, sepBy)
import Utils

fileContent = parseContent $(getFile)

parseContent t = Map.fromList $ unsafeParse (many $ attachedDevices <* "\n") t

attachedDevices = do
  device <- parseDevice
  ": "
  devices <- parseDevice `sepBy` " "
  pure (device, devices)

parseDevice :: Parser String
parseDevice = count 3 (oneOf ['a' .. 'z'])

-- * Generics

-- * FIRST problem

day world = go "you"
  where
    go "out" = 1
    go currentDevice = case Map.lookup currentDevice world of
      Nothing -> 0
      Just otherDevices -> sum (map go otherDevices)

-- * SECOND problem

day' world = go "svr" False False
  where
    go :: String -> Bool -> Bool -> Int
    go "out" True True = 1
    go "out" _ _ = 0
    go currentDevice seenFFT seenDAC = case Map.lookup currentDevice world of
      Nothing -> 0
      Just otherDevices ->
        sum
          ( map
              (\x -> go x (seenFFT || currentDevice == "fft") (seenDAC || currentDevice == "dac"))
              otherDevices
          )

ex =
  parseContent
    """
    aaa: you hhh
    you: bbb ccc
    bbb: ddd eee
    ccc: ddd eee fff
    ddd: ggg
    eee: out
    fff: out
    ggg: out
    hhh: ccc fff iii
    iii: out

    """

ex' = parseContent """
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out

"""

-- started at Sat Dec 13 02:06:51 PM +04 2025
