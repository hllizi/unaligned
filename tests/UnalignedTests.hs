{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.ByteString as BS
import Data.Word
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Unaligned

main :: IO ()
main = hspec $ do
  describe "Unaligned word16-word8 conversion" $ do
    it "leftByte and rightByte invert combineTwoBytes" $
      property $
        \x -> combineTwoBytes (leftByte x) (rightByte x) `shouldBe` (x :: Word16)

  describe "Push a 9-bit word" $ do
    it "precisify later" $ do
      let (bs :> unfinished) =
            pushWord
              ((empty :> Unaligned 128) :: UnalignedBytestring 7)
              ((Unaligned $ 256 + 255 ) :: Unaligned Word16 7)
       in do
            BS.last bs `shouldBe` 255
            unfinished `shouldBe` Unaligned (128 + 64)
