{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.BitString.BigEndian as BitSt
import Data.ByteString.Lazy as BS
import Data.Word
import Debug.Trace
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

    it "Test mergeWord" $ do
      let (bs, unfinished) =
            mergeWord
              (RightOpen 128 1)
              (LeftOpen (256 + 255) 9)
       in do
            BS.last bs `shouldBe` 255
            unfinished `shouldBe` RightOpen (128 + 64) 2

      let (bs, unfinished) =
            mergeWord
              (RightOpen 254 7)
              (LeftOpen 65535 16)
       in do
            BS.last bs `shouldBe` 255
            unfinished `shouldBe` RightOpen 254 7

      let (bs, unfinished) =
            mergeWord
              (RightOpen 0 0)
              (LeftOpen 15 4)
       in do
            bs `shouldBe` empty
            unfinished `shouldBe` RightOpen 240 4

      let (bs, unfinished) =
            mergeWord
              (RightOpen 0 0)
              (LeftOpen 257 9)
       in do
            BS.last bs `shouldBe` 128
            unfinished `shouldBe` RightOpen 128 1

      let (bs, unfinished) =
            mergeWord
              (RightOpen 255 8)
              (LeftOpen 15 4)
       in do
            BS.last bs `shouldBe` 255
            unfinished `shouldBe` RightOpen 240 4

      let target = RightOpen 132 6
          source = LeftOpen 257 9
          firstMerge = mergeWord target source
          secondMerge = mergeWord (snd firstMerge) source
       in do
            firstMerge `shouldBe` (singleton 134, RightOpen 2 7)
            secondMerge `shouldBe` (3 `cons` 1 `cons` empty, RightOpen 0 0)
      let (bs, unfinished) =
            mergeWord
              (RightOpen 128 1)
              (LeftOpen 512 10)
       in do
            BS.last bs `shouldBe` 192
            unfinished `shouldBe` RightOpen 0 3

      let (bs, unfinished) =
            mergeWord
              (RightOpen 128 1)
              (LeftOpen 1040 11)
       in do
            BS.last bs `shouldBe` 193
            unfinished `shouldBe` RightOpen 0 4

    it "make mask for half of a byte, zeroes left" $ do
      makeMask 4 `shouldBe` 15
      makeMask 4 `shouldBe` 15

    it "test takeWord" $ do
      let unalignedBs1 = makeLeftOpenByteString (1 `BS.cons` 127 `BS.cons` empty) 1 9
          unalignedBs2 = makeLeftOpenByteString (1 `BS.cons` 255 `BS.cons` empty) 1 9
          unalignedBs3 = unalignedBs2 {lobsLengthOfNextWord = 0}
          unalignedBs4 = makeLeftOpenByteString (0 `cons` 128 `cons` 192 `cons` 0 `cons` empty) 8 9
       in do
            takeWord unalignedBs2 `shouldBe` (Just $ 256 + 128 + 127, makeLeftOpenByteString BS.empty 8 9)
            takeWord ((trace $ "Hein: " ++ show (BitSt.bitString (toStrict $ lobsContent unalignedBs2))) unalignedBs3) `shouldBe` (Nothing, unalignedBs3)
            takeWord unalignedBs4 `shouldBe` (Just 1, makeLeftOpenByteString (0 `cons` 192 `cons` 0 `cons` BS.empty) 7 9)
            let lobs = makeLeftOpenByteString (0 `cons` 128 `cons` 96 `cons` 0 `cons` empty) 8 9
                allOnes = makeLeftOpenByteString (pack [255, 255, 255, 255])
                halfHalf = makeLeftOpenByteString (pack [15, 15])
                w1 :. (w2 :. _) = lobs
             in do
                  let takeFirst = takeWord lobs
                  takeFirst `shouldBe` (Just 1, makeLeftOpenByteString (0 `cons` 96 `cons` 0 `cons` empty) 7 9)
                  let takeSecond = takeWord $ snd takeFirst
                  takeSecond `shouldBe` (Just 1, makeLeftOpenByteString (32 `cons` 0 `cons` empty) 6 9)
                  w1 `shouldBe` 1
                  w2 `shouldBe` 1

                  fst  (takeWord $ allOnes 8 10) `shouldBe` Just 1023
                  fst  (takeWord $ allOnes 8 11) `shouldBe` Just 2047
                  fst  (takeWord $ allOnes 8 12) `shouldBe` Just 4095
                  fst  (takeWord $ halfHalf 4 8) `shouldBe` Just 240 

    it "test makeRightOpenByteString" $ do
      makeRightOpenByteString BS.empty 8 `shouldBe` EmptyROBs
      let (bs :> (RightOpen integral n :: RightOpen Word8)) =
            makeRightOpenByteString (2 `cons` 255 `cons` empty) 4
       in do
            integral `shouldBe` 240
            n `shouldBe` 4
            bs `shouldBe` 2 `cons` empty

    it "test leftAlign" $ do
      leftAlign (makeLeftOpenByteString (15 `cons` 240 `cons` BS.empty) 4 0) `shouldBe` 255 `cons` 0 `cons` BS.empty
