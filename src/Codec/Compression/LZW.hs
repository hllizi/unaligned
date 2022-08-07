{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Compression.LZW where

import Control.Monad.State
import Data.Bifunctor
import qualified Data.BitString.BigEndian as BitStr
import Data.ByteString.Lazy as BS
import Data.Either
import Data.Either.Extra
import Data.Functor
import Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Word
import Debug.Trace
import Unaligned

type CodeLength = Int

initialMap =
  Prelude.foldr
    ( \(number :: Word16) map ->
        insert [number] number map
    )
    Map.empty
    [1 .. 255]

data CompressState = CompressState
  { dictState :: CompressDictionaryState,
    unfinishedByte :: RightOpen Word8
  }

data CompressDictionaryState = CompressDictionaryState
  { dictionary :: Map (Word16, Word16) Word16,
    nextCode :: Word16,
    codeLength :: CodeLength,
    dictionaryFull :: Bool
  }

compress :: CodeLength -> ByteString -> ByteString
compress maxCodeLength bs =
  if BS.null bs
    then BS.empty
    else
      evalState
        ( compressWithMap
            (Just (fromIntegral $ BS.head bs))
            (BS.tail bs)
        )
        ( CompressState
            (CompressDictionaryState Map.empty 256 9 False)
            (RightOpen 0 0)
        )
  where
    compressWithMap ::
      Maybe Word16 ->
      ByteString ->
      State CompressState ByteString
    compressWithMap buffer bs
      | BS.null bs = do
        CompressState
          (CompressDictionaryState _ _ codeLength _)
          unfinished <-
          get
        let (completed, RightOpen byte n) =
              maybe
                ("", unfinished)
                ( \bufferContent ->
                    mergeWord
                      unfinished
                      (LeftOpen bufferContent codeLength)
                )
                buffer
        return $
          completed
            `append` if n == 0
              then BS.empty
              else BS.singleton byte
      | otherwise = do
        CompressState
          dictState@(CompressDictionaryState map nextCode codeLength full)
          unfinished <-
          get
        let next = fromIntegral $ BS.head bs :: Word16
         in fromMaybe
              ( compressWithMap
                  (Just next)
                  (BS.tail bs)
              )
              $ do
                bufferContent <- buffer

                let extendedBuffer = (bufferContent, next)
                 in pure $
                      case Map.lookup extendedBuffer map of
                        Just code ->
                          compressWithMap
                            (Just code)
                            (BS.tail bs)
                        Nothing ->
                          let (compressedSnippet, newUnfinished) =
                                mergeBuffer extendedBuffer codeLength unfinished
                              newState =
                                let updatedDictionary@(CompressDictionaryState _ _ codelen full) =
                                      update dictState extendedBuffer
                                 in CompressState
                                          updatedDictionary
                                          newUnfinished
                           in do
                                put newState
                                compressedRest <-
                                  compressWithMap
                                    Nothing
                                    (BS.tail bs)
                                return $ compressedSnippet `append` compressedRest
    mergeBuffer buffer codeLength unfinished =
      let (intermediateAcc, intermediateUnfinished) = mergeHelper (fst buffer) codeLength unfinished
       in (intermediateAcc `append`)
            `first` mergeHelper (snd buffer) codeLength intermediateUnfinished

    mergeHelper = \word codeLength -> flip mergeWord (LeftOpen word codeLength)

    update ::
      CompressDictionaryState -> (Word16, Word16) -> CompressDictionaryState
    update dictState@(CompressDictionaryState map nextCode codeLength isFull) buffer
     | isFull = dictState
     | otherwise = 
      let maxCode = 2 ^ codeLength - 1
          (newCode, newCodeLength, newDictionary, newIsFull)
            | fromIntegral nextCode < maxCode =
                (nextCode + 1, codeLength, insert buffer nextCode map, False)
            | codeLength < maxCodeLength =
                (nextCode + 1, codeLength + 1, insert buffer nextCode map, False)
            | otherwise =
                (nextCode, codeLength, insert buffer nextCode map, True)
       in  CompressDictionaryState
            newDictionary
            newCode
            newCodeLength
            newIsFull

data DecompressState = DecompressState
  { dictState :: DecompressDictionaryState
  }

type DecompressDictionary = Map Word16 (Word16, Word16)

data DecompressDictionaryState = DecompressDictionaryState
  { dictionary :: DecompressDictionary,
    nextCode :: Word16,
    codeLength :: CodeLength,
    isFull :: Bool
  }
  deriving (Show)

decompInitial = DecompressState (DecompressDictionaryState Map.empty 256 9 False)

decompress :: CodeLength -> ByteString -> Either String ByteString
decompress maxCodeLength compressed =
  let input = LeftOpenByteString compressed 8 9
   in evalStateT (decompressHelper input) decompInitial
  where
    mapElem x = Prelude.elem x . elems
    unpackEntry :: DecompressDictionary -> Word16 -> Either String ByteString
    unpackEntry dict word =
      if word < 256
        then return $ BS.singleton $ fromIntegral word
        else do
          (word, byte) <-
            maybeToEither
              ( "Compressed data invalid: no entry for code "
                  <> show word
              )
              (Map.lookup word dict)
          unpackEntry dict word <&> (<> (BS.singleton $ fromIntegral byte))

    decompressHelper ::
      LeftOpenByteString ->
      StateT
        DecompressState
        (Either String)
        ByteString
    decompressHelper (Final lobs) = return BS.empty
    decompressHelper (w :< (Final lobs)) =
      do
        DecompressState
          { dictState =
              DecompressDictionaryState
                { dictionary = decompDict,
                  nextCode = nextcode,
                  codeLength = length
                },
            ..
          } <-
          get
        lift $ unpackEntry decompDict w
    decompressHelper whole@(w1 :< (w2 :< lobs)) =
      do
        DecompressState
          { dictState =
              dictSt@DecompressDictionaryState
                { dictionary = decompDict,
                  nextCode = nextcode,
                  codeLength = length
                },
            ..
          } <-
          get
        let newState@(DecompressDictionaryState _ _ newWordLength isFull) = update dictSt (w1, w2)
        put DecompressState {dictState = newState}
        compressedRest <- decompressHelper (lobs {lobsLengthOfNextWord = newWordLength})
        lift $
          (<>)
            <$> unpackEntry decompDict w1
            <*> (unpackEntry decompDict w2 <&> (<> compressedRest))

    update :: DecompressDictionaryState -> (Word16, Word16) -> DecompressDictionaryState
    update dictState@(DecompressDictionaryState map nextCode codeLength isFull) pair
     | isFull = dictState
     | otherwise = 
         let  maxCode = 2 ^ codeLength - 1 
              (newCode, newCodeLength, newDictionary, newIsFull)
                | nextCode < maxCode =
                    (nextCode + 1, codeLength, insert nextCode pair map, False)
                | codeLength < maxCodeLength =
                    (nextCode + 1, codeLength + 1, insert nextCode pair map, False)
                | otherwise = 
                    (nextCode, codeLength, map, True)
          in DecompressDictionaryState
                 newDictionary
                 (traceThisPrefixed "new code: " newCode)
                 newCodeLength
                 newIsFull

--         maybeToEither $
--             case Map.lookup currentWord (decompressDictionary $ dictState decompressState) of
--                Just word -> undefined
--                Nothing -> undefined
--
--

traceThisPrefixed :: (Show a) => String -> a -> a
traceThisPrefixed p x = trace (p <> (show x)) x
