{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Compression.LZW
import Data.BitString.BigEndian
import Data.ByteString.Lazy as BS
import Data.Either
import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Unaligned

main :: IO ()
main = hspec $ do
    it "Test compress" $ do
      let toCompress = 1 `cons` 1 `cons` 1 `cons` 1 `cons` BS.empty
          compressed = compress 9 toCompress
          expected = 0 `cons` 128 `cons` 96 `cons` 0 `cons` BS.empty
       in trace ("result:   " ++ show (bitString $ toStrict compressed)) $
            compressed
              `shouldBe` trace ("expected: " ++ show (bitString $ toStrict expected)) expected

    it "Test decompress" $ do
      let woppes = (decompress 9) . (compress 9) $ "Hallo, Spencer!"
--      (fromRight "Default" $
--        trace (show woppes) woppes)
--          `shouldBe` "Hallo, Spencer!"
      (fromRight "Default" $ decompress 9 (0 `cons` 128 `cons` 192 `cons` 0 `cons` BS.empty))
        `shouldBe` pack [1, 1, 1, 1]
