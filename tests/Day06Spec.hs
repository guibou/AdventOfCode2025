{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day06Spec where

import Day06
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 4277556
    it "of second star" $ do
      day' ex `shouldBe` 3263827
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4387670995909
    it "on second star" $ do
      day' fileContent `shouldBe` 9625320374409
