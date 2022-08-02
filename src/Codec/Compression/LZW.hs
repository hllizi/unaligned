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
import Data.ByteString.Lazy as BS
import Data.Either
import Data.Either.Extra
import Data.Functor
import Data.Map as Map
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
        return $
          completed
            `append` if n == 0
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
                                put (trace (show newUnfinished) newState)
                                compressedRest <-
                                  compressWithMap
                                    Nothing
                                    (BS.tail bs)
                                return $ compressedSnippet `append` compressedRest
              )
              buffer
    mergeBuffer buffer unfinished =
      let (intermediateAcc, intermediateUnfinished) = trace ("Merging " <> show buffer <> show unfinished) mergeHelper (fst buffer) unfinished
       in (intermediateAcc `append`)
            `first` mergeHelper (snd buffer) intermediateUnfinished

    mergeHelper = \word -> flip mergeWord (LeftOpen word codeLength)
    update ::
      CompressDictionaryState -> (Word16, Word16) -> CompressDictionaryState
    update dictState@(CompressDictionaryState map nextCode) buffer =
      if nextCode <= 2 ^ codeLength - 1
        then
          let newDictionary = insert buffer nextCode map
           in CompressDictionaryState
                (trace ("Comp dict: " <> show newDictionary) newDictionary)
                (nextCode + 1)
        else dictState

data DecompressState = DecompressState
  { dictState :: DecompressDictionaryState
  }

type DecompressDictionary = Map Word16 (Word16, Word16)

data DecompressDictionaryState = DecompressDictionaryState
  { dictionary :: DecompressDictionary,
    nextCode :: Word16
  }
  deriving (Show)

decompInitial = DecompressState (DecompressDictionaryState Map.empty 256)

decompress :: CodeLength -> ByteString -> Either String ByteString
decompress codeLength compressed =
  let input = LeftOpenByteString compressed 8 codeLength
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
                  nextCode = nextcode
                },
            ..
          } <-
          get
        lift $ unpackEntry decompDict w
    decompressHelper (w1 :< (w2 :< lobs)) =
      do
        DecompressState
          { dictState =
              dictSt@DecompressDictionaryState
                { dictionary = decompDict,
                  nextCode = nextcode
                },
            ..
          } <-
          get
        let newState = update dictSt (w1, w2)
        put DecompressState {dictState = newState}
        compressedRest <- decompressHelper lobs
        lift $
          (<>)
            <$> unpackEntry decompDict w1
            <*> (unpackEntry decompDict w2 <&> (<> compressedRest))

    update :: DecompressDictionaryState -> (Word16, Word16) -> DecompressDictionaryState
    update dictState@(DecompressDictionaryState map nextCode) pair =
      if nextCode <= 2 ^ codeLength - 1
        then
          let newDictionary = insert nextCode pair map
           in DecompressDictionaryState
                (trace ("Decomp dict: " <> show newDictionary) newDictionary)
                (nextCode + 1)
        else dictState

--         maybeToEither $
--             case Map.lookup currentWord (decompressDictionary $ dictState decompressState) of
--                Just word -> undefined
--                Nothing -> undefined
--
--
