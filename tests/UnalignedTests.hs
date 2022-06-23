{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.ByteString as BS
import Data.Word
import Test.Hspec
import Test.QuickCheck
import Unaligned

main :: IO ()
main = hspec $ do
  describe "Test Unaligned module" $ do
    it "leftByte and rightByte invert combineTwoBytes" $
      property $
        \x -> combineTwoBytes (leftByte x) (rightByte x) `shouldBe` (x :: Word16)

    it "Test smart constructors" $ do
      makeLeftOpen (65535 :: Word16) 8
        `shouldBe` LeftOpen 255 8
      makeRightOpen (65535 :: Word16) 11
        `shouldBe` RightOpen 65504 11

    it "Test the results of pushing an unaligned word onto a ByteString" $ do
      let (bs :> unfinished) =
            pushWord
              (empty :> RightOpen 128 1)
              (LeftOpen (256 + 255) 9)
       in do
            BS.last bs `shouldBe` 255
            unfinished `shouldBe` RightOpen (128 + 64) 2

      let (bs :> unfinished) =
            pushWord
              (empty :> RightOpen 254 7)
              (LeftOpen 65535 16)
       in do
            BS.last bs `shouldBe` 255
            unfinished `shouldBe` RightOpen 254 7
      let  (bs :> unfinished) =
            pushWord
                Empty
                (LeftOpen 15 4)
       in do 
           bs `shouldBe` empty 
           unfinished `shouldBe` RightOpen 240 4

      let  (bs :> unfinished) =
            pushWord
                Empty
                (LeftOpen 257 9)
       in do 
           BS.last bs `shouldBe` 128 
           unfinished `shouldBe` RightOpen 128 1


    it "make mask for half of a byte, zeroes left" $ do
      makeMask 4 `shouldBe` 15
      makeMask 4 `shouldBe` 15

    it "test takeWord" $ do
      let unalignedBs1 = (LeftOpen 1 1 :< (127 `BS.cons` empty))
          unalignedBs2 = (LeftOpen 1 1 :< (255 `BS.cons` empty))
       in do
            takeWord unalignedBs1 9 `shouldBe` Just (256 + 127)
            takeWord unalignedBs2 9 `shouldBe` Just (256 + 128 + 127)

    it "test minBytes" $ do
      minBytes 7 `shouldBe` 1
      minBytes 8 `shouldBe` 1
      minBytes 9 `shouldBe` 2

    it "test makeRightOpenByteString" $ do
      makeRightOpenByteString BS.empty 8 `shouldBe` Nothing
      let Just (bs :> (RightOpen integral n :: RightOpen Word8)) =
            makeRightOpenByteString (2 `cons` 255 `cons` empty) 4
       in do
            integral `shouldBe` 240
            n `shouldBe` 4
            bs `shouldBe` 2 `cons` empty

    it "test makeUnaligned LeftOpen ByteString" $ do
      makeLeftOpenByteString BS.empty 8 `shouldBe` Nothing
      let Just ((LeftOpen integral n) :< bs) =
            makeLeftOpenByteString  (255 `cons` 2 `cons` empty) 4
       in do
            integral `shouldBe` 15
            n `shouldBe` 4
            bs `shouldBe` 2 `cons` empty
