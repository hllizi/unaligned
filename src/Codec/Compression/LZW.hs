{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Compression.LZW where

import Control.Monad.State
import Data.Bifunctor
import Data.ByteString as BS
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
    acc :: ByteString,
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
      maybe "" toByteString
        (evalState
          ( compressWithMap
              (Just (fromIntegral $ BS.head bs))
              (BS.tail bs)
          )
          ( CompressState
              (CompressDictionaryState Map.empty 256)
              ""
              (RightOpen 0 0)
          ))
  where
    compressWithMap ::
      Maybe Word16 ->
      ByteString ->
      State CompressState (Maybe (ByteString, RightOpen Word8))
    compressWithMap buffer bs
      | BS.null bs = do
        CompressState _ acc unfinished <- get
        return $
          ( \bufferContent ->
              (acc `append`) `first` 
                mergeWord
                 unfinished
                 (LeftOpen bufferContent codeLength)
          )
            <$> buffer
      | otherwise = do
        CompressState dictState@(CompressDictionaryState map nextCode) acc unfinished <-
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
                          let (accExtension, newUnfinished) =
                                mergeBuffer extendedBuffer unfinished
                              newState =
                                let updatedDictionary =
                                      update dictState extendedBuffer
                                 in CompressState
                                      updatedDictionary
                                      (acc `append` accExtension)
                                      newUnfinished
                           in do
                                put newState
                                compressWithMap
                                  Nothing
                                  (BS.tail bs)
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
