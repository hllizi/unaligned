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
import Data.Word
import Data.Char
import Data.Binary.Builder
import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Unaligned
import qualified Codec.Compression.LZW.Conduit as Cond
import Conduit
import Data.Binary.Builder

main :: IO ()
main = do
  sampletext <- readFile "./tests/sample.txt"
  let testText = BS.pack $ map (fromIntegral . ord) sampletext
  hspec $ do
    it "Test compress" $ do
      let toCompress = 1 `BS.cons` 1 `BS.cons` 1 `BS.cons` 1 `BS.cons` BS.empty
          compressed = compress 12 toCompress
          expected = 0 `BS.cons` 128 `BS.cons` 96 `BS.cons` 0 `BS.cons` BS.empty
       in compressed `shouldBe` expected

    it "decompress inverts compress" $
      property $ \(bytes :: [Word8]) -> bytes == (BS.unpack . decompress 12 $ compress 12 $ BS.pack bytes)

    it "Test decompress" $ do
      let compressed = compress 11 testText
          decompressed = decompress 11 compressed
       in do
            decompressed
              `shouldBe` testText
            forM_
              [testText]
              ( \testText ->
                  return
                    ( ( 
                            decompress 12
                          . compress 12
                          $ testText
                      )
                        `shouldBe` testText
                    )
              )
      decompress 12 (BS.pack [0, 128, 96, 0])
        `shouldBe` BS.pack [1, 1, 1, 1]

      
    it "Test conduit inverse" $ do
        compressDecompress <- 
         runConduit $   yield testText 
                     .| Cond.compress 13
                     .| Cond.decompress 13
                     .| mapC fromLazyByteString
                     .| sinkLazyBuilder

                      

        compressDecompress `shouldBe` testText


