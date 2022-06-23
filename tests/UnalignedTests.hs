{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
      makeUnaligned (65535 :: Word16) 8
        `shouldBe` (Unaligned (255 :: Word16) 8 :: (Unaligned 'LeftOpen Word16))
      makeUnaligned (65535 :: Word16) 11
        `shouldBe` (Unaligned (65504 :: Word16) 11 :: (Unaligned 'RightOpen Word16))

    it "Test the results of pushing an unaligned word onto a ByteString" $ do
      let (bs :> unfinished) =
            pushWord
              ((empty :> Unaligned 128 1) :: UnalignedBytestring 'RightOpen)
              (Unaligned (256 + 255) 9 :: Unaligned 'LeftOpen Word16)
       in do
            BS.last bs `shouldBe` 255
            unfinished `shouldBe` Unaligned (128 + 64) 2

      let (bs :> unfinished) =
            pushWord
              ((empty :> Unaligned 254 7) :: UnalignedBytestring 'RightOpen)
              (Unaligned 65535 16 :: Unaligned 'LeftOpen Word16)
       in do
            BS.last bs `shouldBe` 255
            unfinished `shouldBe` Unaligned 254 7

    it "make mask for half of a byte, zeroes left" $ do
      makeMask 4 `shouldBe` 15
      makeMask 4 `shouldBe` 15

    it "test takeWord" $ do
      let unalignedBs1 = (Unaligned 1 1 :< (127 `BS.cons` empty))
          unalignedBs2 = (Unaligned 1 1 :< (255 `BS.cons` empty))
       in do
            takeWord unalignedBs1 9 `shouldBe` Just (256 + 127)
            takeWord unalignedBs2 9 `shouldBe` Just (256 + 128 + 127)

    it "test minBytes" $ do
      minBytes 7 `shouldBe` 1
      minBytes 8 `shouldBe` 1
      minBytes 9 `shouldBe` 2

    it "test makeUnaligned RightOpen ByteString" $ do
      makeUnalignedByteString @(UnalignedBytestring 'RightOpen) BS.empty 8 `shouldBe` Nothing
      let Just (bs :> (Unaligned integral n :: Unaligned 'RightOpen Word8)) =
            makeUnalignedByteString @(UnalignedBytestring 'RightOpen)(2 `cons` 255 `cons` empty) 4
       in do
            integral `shouldBe` 240
            n `shouldBe` 4
            bs `shouldBe` 2 `cons` empty

    it "test makeUnaligned LeftOpen ByteString" $ do
      makeUnalignedByteString @(UnalignedBytestring 'LeftOpen) BS.empty 8 `shouldBe` Nothing
      let Just ((Unaligned integral n :: Unaligned 'LeftOpen Word8) :< bs) =
            makeUnalignedByteString @(UnalignedBytestring 'LeftOpen) (255 `cons` 2 `cons` empty) 4
       in do
            integral `shouldBe` 15
            n `shouldBe` 4
            bs `shouldBe` 2 `cons` empty
