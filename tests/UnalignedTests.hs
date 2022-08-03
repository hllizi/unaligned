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
      let (bs :> unfinished) =
            pushWord
              EmptyROBs
              (LeftOpen 15 4)
       in do
            bs `shouldBe` empty
            unfinished `shouldBe` RightOpen 240 4

      let (bs :> unfinished) =
            pushWord
              EmptyROBs
              (LeftOpen 257 9)
       in do
            BS.last bs `shouldBe` 128
            unfinished `shouldBe` RightOpen 128 1

      let base = EmptyROBs
          first = LeftOpen 1 9
          second = LeftOpen 256 9
          result = pushWord (pushWord (pushWord base first) first) second
          expected = (0 `cons` 128 `cons` 96 `cons` BS.empty) :> RightOpen 0 3
       in trace
            ( "result  : "
                ++ show
                  ( BitSt.bitString
                      (toStrict $ toByteString result)
                  )
            )
            result
            `shouldBe` trace
              ( "expected: "
                  ++ show
                    ( BitSt.bitString
                        (toStrict $ toByteString expected)
                    )
              )
              expected
    it "make mask for half of a byte, zeroes left" $ do
      makeMask 4 `shouldBe` 15
      makeMask 4 `shouldBe` 15

    it "test takeWord" $ do
      let unalignedBs1@(LeftOpenByteString bs1 n1 _) = (LeftOpenByteString (1 `BS.cons` 127 `BS.cons` empty) 1 9)
          unalignedBs2@(LeftOpenByteString bs2 n2 _) = (LeftOpenByteString (1 `BS.cons` 255 `BS.cons` empty) 1 9)
          unalignedBs3 = unalignedBs2 {lobsLengthOfNextWord = 0}
          unalignedBs4 = LeftOpenByteString (0 `cons` 128 `cons` 192 `cons` 0 `cons` empty) 8 9
       in do
            --     takeWord ((trace $ "Bo: " ++ show (BitSt.bitString (toStrict bs1))) unalignedBs1) 9 `shouldBe` Just (256 + 127, LeftOpenByteString BS.empty 0)
            takeWord ((trace $ "Hein: " ++ show (BitSt.bitString (toStrict bs2))) unalignedBs2) `shouldBe` (Just $ 256 + 128 + 127, LeftOpenByteString BS.empty 8 9)
            takeWord ((trace $ "Hein: " ++ show (BitSt.bitString (toStrict bs2))) unalignedBs3) `shouldBe` (Nothing, unalignedBs3)
            takeWord unalignedBs4 `shouldBe` (Just 1, LeftOpenByteString (0 `cons` 192 `cons` 0 `cons` BS.empty) 7 9)
            let lobs = LeftOpenByteString (0 `cons` 128 `cons` 96 `cons` 0 `cons` empty) 8 9
                w1 :< (w2 :< _) = lobs
             in do
                  let takeFirst@(_, LeftOpenByteString bs _ _) = takeWord lobs
                  takeFirst `shouldBe` (trace ("Knood " <> (show $ BitSt.bitString $ toStrict $ bs)) (Just 1, LeftOpenByteString (0 `cons` 96 `cons` 0 `cons` empty) 7 9))
                  let takeSecond@(w, LeftOpenByteString _ _ _) = takeWord $ snd takeFirst
                  takeSecond `shouldBe` (trace ("Knood " <> (show $ BitSt.bitString $ toStrict $ bs)) (Just 1, LeftOpenByteString (32 `cons` 0 `cons` empty) 6 9))
                  w1 `shouldBe` 1
                  w2 `shouldBe` 1

    it "test minBytes" $ do
      minBytes 7 `shouldBe` 1
      minBytes 8 `shouldBe` 1
      minBytes 9 `shouldBe` 2

    it "test makeRightOpenByteString" $ do
      makeRightOpenByteString BS.empty 8 `shouldBe` EmptyROBs
      let (bs :> (RightOpen integral n :: RightOpen Word8)) =
            makeRightOpenByteString (2 `cons` 255 `cons` empty) 4
       in do
            integral `shouldBe` 240
            n `shouldBe` 4
            bs `shouldBe` 2 `cons` empty

    it "test leftAlign" $ do
      leftAlign (LeftOpenByteString (15 `cons` 240 `cons` BS.empty) 4 0) `shouldBe` 255 `cons` 0 `cons` BS.empty
