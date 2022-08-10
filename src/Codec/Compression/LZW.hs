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
import qualified Data.Vector as V
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
  { dictState :: DictionaryState,
    unfinishedByte :: RightOpen Word8
  }

data DictionaryState = DictionaryState
  { dictionary :: Dictionary,
    nextCode :: Word16,
    codeLength :: CodeLength,
    maxCode :: Word16,
    isFull :: Bool
  }

makeDictionaryState dictionary nextCode codeLength =
  DictionaryState dictionary nextCode codeLength (2 ^ codeLength - 1)

data Dictionary
  = CompressDictionary (Map (Word16, Word16) Word16)
  | DecompressDictionary (Map Word16 (Word16, Word16))

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
            (makeDictionaryState (CompressDictionary Map.empty) 256 9 False)
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
          (DictionaryState _ _ codeLength _ _)
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
          dictState@(DictionaryState wrappedDictionary nextCode codeLength _ _)
          unfinished <-
          get
        let dictionary = case wrappedDictionary of
              CompressDictionary map -> map
              DecompressDictionary _ ->
                error "A decompress dictionary was used in the function compress. This should not even be possible."

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
                      case Map.lookup extendedBuffer dictionary of
                        Just code ->
                          compressWithMap
                            (Just code)
                            (BS.tail bs)
                        Nothing ->
                          let (compressedSnippet, newUnfinished) =
                                mergeBuffer extendedBuffer codeLength unfinished
                              newState =
                                let updatedDictionary@(DictionaryState _ _ _ _ _) =
                                      updateDictionary dictState extendedBuffer maxCodeLength
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

updateDictionary ::
  DictionaryState ->
  (Word16, Word16) ->
  CodeLength ->
  DictionaryState
updateDictionary dictState@(DictionaryState dictionary nextCode codeLength maxCode isFull) pair maxCodeLength
  | isFull = dictState
  | otherwise =
    let (newCode, newCodeLength, newDictionary, newMaxCode, newIsFull)
          | fromIntegral nextCode < maxCode =
            (nextCode + 1, codeLength, updatedDictionary, maxCode, False)
          | codeLength < maxCodeLength =
            let newCodeLength = codeLength + 1
                newMaxCode = 2 ^ newCodeLength - 1
             in (nextCode + 1, newCodeLength, updatedDictionary, newMaxCode, False)
          | otherwise =
            (nextCode, codeLength, updatedDictionary, maxCode, True)
     in traceShow
          "update"
          DictionaryState
          newDictionary
          newCode
          newCodeLength
          newMaxCode
          newIsFull
  where
    updatedDictionary = case dictionary of
      CompressDictionary map -> CompressDictionary $ insert pair nextCode map
      DecompressDictionary map -> DecompressDictionary $ updateDict nextCode pair map

updateDict = insert
newtype DecompressState = DecompressState
  { dictState :: DictionaryState
  }

type DecompressDictionary = Map Word16 (Word16, Word16)

data DecompressDictionaryState = DecompressDictionaryState
  { dictionary :: DecompressDictionary,
    nextCode :: Word16,
    codeLength :: CodeLength,
    maxCode :: Word16,
    isFull :: Bool
  }
  deriving (Show)

decompInitial = DecompressState (makeDictionaryState (DecompressDictionary Map.empty) 256 9 False)

decompress :: CodeLength -> ByteString -> Either String ByteString
decompress maxCodeLength compressed =
  let input = makeLeftOpenByteString compressed 8 9
   in evalStateT (decompressHelper input) decompInitial
  where
    mapElem x = Prelude.elem x . elems
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
              DictionaryState
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
              dictSt@DictionaryState
                { dictionary = decompDict,
                  nextCode = nextcode,
                  codeLength = length
                },
            ..
          } <-
          get
        let newState@(DictionaryState _ _ newWordLength _ isFull) = updateDictionary dictSt (w1, w2) maxCodeLength
        put DecompressState {dictState = newState}
        compressedRest <- decompressHelper (lobs {lobsLengthOfNextWord = newWordLength})
        lift $
          (<>)
            <$> unpackEntry decompDict w1
            <*> (Right (BS.singleton $ rightByte w2) <&> (<> compressedRest))

unpackEntry ::
  Dictionary ->
  Word16 ->
  Either String ByteString
unpackEntry dictionary word =
  case dictionary of
    DecompressDictionary map ->
      if word < 256
        then return $ BS.singleton $ fromIntegral word
        else do
          (word, byte) <-
            maybeToEither
              ( "Compressed data invalid: no entry for code "
                  <> show word
              )
              (Map.lookup word map)
          unpackEntry dictionary word <&> (<> (BS.singleton $ fromIntegral byte))
    CompressDictionary _ ->
      Left "A decompress dictionary was provided to the function unpackEntry in the function decompress. This should not even be possible."
