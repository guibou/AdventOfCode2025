{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day03Spec where

import Day03
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 357
    it "of second star" $ do
      day' ex `shouldBe` 3121910778619
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 17324
    it "on second star" $ do
      day' fileContent `shouldBe` 171846613143331
