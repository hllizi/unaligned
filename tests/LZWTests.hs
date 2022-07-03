{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.ByteString.Lazy as BS
import Data.Word
import Test.Hspec
import Test.QuickCheck
import Unaligned
import Codec.Compression.LZW
import Data.BitString.BigEndian
import Debug.Trace

main :: IO ()
main = hspec $ do
  describe "Test LZW module" $ do

    it "Test compress" $ do
      let toCompress = 1 `cons` 1 `cons` 1 `cons` 1 `cons` BS.empty
          compressed = compress 9 toCompress 
          expected = 0 `cons` 128 `cons` 96 `cons` 0 `cons` BS.empty

       in 
                   trace  ("result:   " ++ show (bitString $ toStrict compressed)) $ compressed 
        `shouldBe` trace ("expected: " ++ show (bitString $ toStrict expected)) expected


