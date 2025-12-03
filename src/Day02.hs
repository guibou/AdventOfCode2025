module Day02 where

import Data.Text qualified as Text
import Utils

fileContent = parseContent $(getFile)

parseContent :: Text -> [(Int, Int)]
parseContent t =
  fmap
    ( \w ->
        let (a, b) = Text.break (== '-') w
         in ( read $ Text.unpack a,
              read $ drop 1 $ Text.unpack b
            )
    )
    $ Text.split (== ',') t

-- * Generics

isInvalid i = do
  let t = Text.pack (show i)
  let (a, b) = Text.splitAt (Text.length t `div` 2) t
  a == b

-- * FIRST problem

day l = sum $ concatMap (\(a, b) -> filter isInvalid [a .. b]) l

-- * SECOND problem

isInvalid' i = do
  let t = Text.pack (show i)
  any
    ( \s ->
        let (chunk : chunks) = Text.chunksOf s t
         in all (== chunk) $ chunks
    )
    [1 .. (Text.length t `div` 2)]

day' l = sum $ concatMap (\(a, b) -> filter isInvalid' [a .. b]) l

ex =
  parseContent
    """
    11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
    1698522-1698528,446443-446449,38593856-38593862,565653-565659,
    824824821-824824827,2121212118-2121212124

    """

-- started at Wed Dec  3 07:23:45 AM +04 2025
-- end at 07:38
