{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day02Spec where

import Day02
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 1227775554
    it "of second star" $ do
      day' ex `shouldBe` 4174379265
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 20223751480
    it "on second star" $ do
      day' fileContent `shouldBe` 30260171216
