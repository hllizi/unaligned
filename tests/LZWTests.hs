{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Compression.LZW
import Control.Monad
import Data.BitString.BigEndian
import qualified Data.ByteString.Lazy as BS
import Data.Either
import Data.List
import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Unaligned

main :: IO ()
main = hspec $ do
  it "Test compress" $ do
    let toCompress = 1 `BS.cons` 1 `BS.cons` 1 `BS.cons` 1 `BS.cons` BS.empty
        compressed = compress 9 toCompress
        expected = 0 `BS.cons` 128 `BS.cons` 96 `BS.cons` 0 `BS.cons` BS.empty
     in compressed `shouldBe` expected

  it "Test decompress" $ do
    let testText = "Hallo, Spencer!"
        compressed = compress 9 testText
        decompressed = (decompress 9) $ trace ("Boou: " <> (show $ bitString $ BS.toStrict compressed)) compressed 
     in do
          (fromRight "Default" decompressed)
            `shouldBe` testText
          forM_
            [testText]
            ( \testText ->
                return
                  ( ( fromRight "Default"
                        . decompress 9
                        . compress 9
                        $ testText
                    )
                      `shouldBe` testText
                  )
            )
    fromRight "Default" (decompress 9 (BS.pack [0, 128, 96, 0]))
      `shouldBe` BS.pack [1, 1, 1, 1]

showBytewise = intercalate ", " . map show . BS.unpack
