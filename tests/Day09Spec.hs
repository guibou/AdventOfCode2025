{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day09Spec where

import Day09
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 50
    it "of second star" $ do
      day' ex `shouldBe` 24
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4759531084
    it "on second star" $ do
      day' fileContent `shouldBe` 0
