{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Compression.LZW where

import Control.Monad.State
import Data.Bifunctor
import Data.ByteString.Lazy as BS
import Data.Functor
import Data.Map as Map
import Data.Proxy
import Data.Word
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
    nextCode :: Word16
  }

compress :: CodeLength -> ByteString -> ByteString
compress codeLength bs =
  if BS.null bs
    then BS.empty
    else
       evalState
          ( compressWithMap
              (Just (fromIntegral $ BS.head bs))
              (BS.tail bs)
          )
          ( CompressState
              (CompressDictionaryState Map.empty 256)
              (RightOpen 0 0)
          )
  where
    compressWithMap ::
      Maybe Word16 ->
      ByteString ->
      State CompressState ByteString
    compressWithMap buffer bs
      | BS.null bs = do
        CompressState _ unfinished <- get
        let (completed, RightOpen byte n) =
              maybe
                ("", unfinished)
                ( \bufferContent ->
                    mergeWord
                      unfinished
                      (LeftOpen bufferContent codeLength)
                )
                buffer
        return $ completed `append` 
            if n == 0
                then BS.empty
                else BS.singleton byte
      | otherwise = do
        CompressState
          dictState@(CompressDictionaryState map nextCode)
          unfinished <-
          get
        let next = fromIntegral $ BS.head bs :: Word16
         in maybe
              ( compressWithMap
                  (Just next)
                  (BS.tail bs)
              )
              ( \bufferContent ->
                  let extendedBuffer = (bufferContent, next)
                   in case Map.lookup extendedBuffer map of
                        Just code ->
                          compressWithMap
                            (Just code)
                            (BS.tail bs)
                        Nothing ->
                          let (compressedSnippet, newUnfinished) =
                                mergeBuffer extendedBuffer unfinished
                              newState =
                                let updatedDictionary =
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
              )
              buffer
    mergeBuffer buffer unfinished =
      let (intermediateAcc, intermediateUnfinished) = mergeHelper (fst buffer) unfinished
       in (intermediateAcc `append`)
            `first` mergeHelper (snd buffer) intermediateUnfinished

    mergeHelper = \word -> flip mergeWord (LeftOpen word codeLength)
    update ::
      CompressDictionaryState -> (Word16, Word16) -> CompressDictionaryState
    update dictState@(CompressDictionaryState map nextCode) buffer =
      if nextCode <= 2 ^ codeLength - 1
        then
          CompressDictionaryState
            (insert buffer nextCode map)
            (nextCode + 1)
        else dictState

data DecompressState = DecompressState
  { dictState :: DecompressDictionaryState,
    acc :: BS.ByteString
  }

data DecompressDictionaryState = DecompressDictionaryState
  { dictionary :: Map (Word16, Word16) Word16,
    nextCode :: Word16
  }

decompInitial = DecompressState (DecompressDictionaryState Map.empty 256) BS.empty

decompress :: CodeLength -> ByteString -> ByteString
decompress codeLength compressed =
  let input = makeLeftOpenByteString compressed 0
   in evalState (decompressHelper input) decompInitial
  where
    decompressHelper :: LeftOpenByteString -> State DecompressState ByteString
    decompressHelper input =
      let maybeNextBytes =
            do
              word <- takeWord input codeLength
              undefined
       in undefined
