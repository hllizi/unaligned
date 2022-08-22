{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Compression.LZW where

import Control.Exception
import Control.Monad.ST.Lazy
import Control.Monad.State.Lazy
import Data.Bifunctor
import qualified Data.BitString.BigEndian as BitStr
import Data.ByteString.Lazy as BS
import Data.Either
import Data.Either.Extra
import Data.Functor
import qualified Data.HashMap as M
import Data.Maybe
import Data.Proxy
import Data.Typeable
import qualified Data.Vector as A
import qualified Data.Vector.Mutable as M
import Data.Word
import Debug.Trace
import Unaligned

type CodeLength = Int

initialMap =
  Prelude.foldr
    ( \(number :: Word16) map ->
        M.insert [number] number map
    )
    M.empty
    [1 .. 255]

data CompressState s = CompressState
  { compressDictState :: !(DictionaryState s),
    unfinishedByte :: !(RightOpen Word8)
  }

data DictionaryState s = DictionaryState
  { dictionary :: !(Dictionary s),
    nextCode :: !Word16,
    codeLength :: !CodeLength,
    maxCode :: !Word16,
    isFull :: !Bool
  }

makeDictionaryState :: Dictionary s -> Word16 -> CodeLength -> Bool -> (DictionaryState s)
makeDictionaryState dictionary nextCode codeLength =
  DictionaryState dictionary nextCode codeLength (2 ^ codeLength - 1)

data Dictionary s
  = CompressDictionary (M.Map (Word16, Word16) Word16)
  | DecompressDictionary !(M.STVector s (Maybe (Word16, Word16)))

compress :: CodeLength -> ByteString -> ByteString
compress maxCodeLength bs =
  if BS.null bs
    then BS.empty
    else
      runST $
        evalStateT
          ( compressWithMap
              (Just (fromIntegral $ BS.head bs))
              (BS.tail bs)
          )
          ( CompressState
              (makeDictionaryState (CompressDictionary M.empty) 256 9 False)
              (RightOpen 0 0)
          )
  where
    compressWithMap ::
      Maybe Word16 ->
      ByteString ->
      StateT
        (CompressState s)
        (ST s)
        ByteString
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
                      case M.lookup extendedBuffer dictionary of
                        Just code ->
                          compressWithMap
                            (Just code)
                            (BS.tail bs)
                        Nothing ->
                          let (compressedSnippet, newUnfinished) =
                                mergeBuffer extendedBuffer codeLength unfinished
                           in do
                                updatedDictionary <-
                                  lift $
                                    updateDictionary
                                      dictState
                                      extendedBuffer
                                      maxCodeLength
                                let newState =
                                      CompressState
                                        updatedDictionary
                                        newUnfinished
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
  forall s.
  DictionaryState s ->
  (Word16, Word16) ->
  CodeLength ->
  ST s (DictionaryState s)
updateDictionary dictState@(DictionaryState dictionary nextCode codeLength maxCode isFull) pair maxCodeLength
  | isFull = pure dictState
  | otherwise = do
    updatedDictionary <- updatedDictionaryMon
    let (newCode, newCodeLength, newDictionary, newMaxCode, newIsFull)
          | fromIntegral nextCode < maxCode =
            (nextCode + 1, codeLength, updatedDictionary, maxCode, False)
          | codeLength < maxCodeLength =
            let newCodeLength = codeLength + 1
                newMaxCode = 2 ^ newCodeLength - 1
             in (nextCode + 1, newCodeLength, updatedDictionary, newMaxCode, False)
          | otherwise =
            (nextCode, codeLength, updatedDictionary, maxCode, True)
     in pure $
          DictionaryState
            newDictionary
            newCode
            newCodeLength
            newMaxCode
            newIsFull
  where
    updatedDictionaryMon :: ST s (Dictionary s)
    updatedDictionaryMon = case dictionary of
      CompressDictionary map -> pure $ CompressDictionary $ M.insert pair nextCode map
      DecompressDictionary array ->
        DecompressDictionary <$> do
          M.write array (fromIntegral nextCode) (Just pair)
          bamboon <- A.freeze array
          --pure $ trace ("boonish: " <> show bamboon) array
          pure array

newtype DecompressState s = DecompressState
  { decompressDictState :: DictionaryState s
  }

type DecompressDictionary = M.Map Word16 (Word16, Word16)

decompress :: CodeLength -> ByteString -> ByteString
decompress maxCodeLength compressed =
  let input = makeLeftOpenByteString compressed 8 9
      initialDecompressionState :: ST s (DecompressState s)
      initialDecompressionState =
        do
          let maxNumberOfCodes = 2 ^ maxCodeLength
          prefilledVector <-
            M.replicate maxNumberOfCodes Nothing
          return
            ( DecompressState
                ( makeDictionaryState
                    ( DecompressDictionary
                        prefilledVector
                    )
                    256
                    9
                    False
                )
            )
   in runST $ do
        initState <- initialDecompressionState
        evalStateT (decompressHelper input) initState
  where
    mapElem x = Prelude.elem x . M.elems
    decompressHelper ::
      LeftOpenByteString ->
      StateT
        (DecompressState s)
        (ST s)
        ByteString
    decompressHelper (Final lobs) = return BS.empty
    decompressHelper (w :< (Final lobs)) =
      do
        DecompressState
          { decompressDictState =
              DictionaryState
                { dictionary = decompDict,
                  nextCode = nextcode,
                  codeLength = length
                },
            ..
          } <-
          get
        unpackEntry (fromIntegral w)
    decompressHelper whole@(w1 :< (w2 :< lobs)) =
      do
        DecompressState
          { decompressDictState =
              dictSt@DictionaryState
                { dictionary = decompDict,
                  nextCode = nextcode,
                  codeLength = length
                },
            ..
          } <-
          get
        newState@( DictionaryState
                     dictionary
                     _
                     newWordLength
                     _
                     isFull
                   ) <-
          lift $ updateDictionary dictSt (w1, w2) maxCodeLength

        frozen <- case dictionary of
          DecompressDictionary dict -> A.freeze dict
        -- trace ("Frozen out: " <> show frozen) $ return ()

        put DecompressState {decompressDictState = newState}
        compressedRest <- decompressHelper (lobs {lobsLengthOfNextWord = newWordLength})
        codeUnpacked <- trace "Mordnsknut" unpackEntry (fromIntegral w1)
        return $ codeUnpacked 
              <> BS.singleton (rightByte w2)
              <> compressedRest

    -- without the following line, hls complains about non-exhaustive pattern matches even though the use of the actually exported (non-constructor) patterns should is exhaustive.
    decompressHelper x = error $ "This should not have happened: " <> show x <> " did not match any pattern built with :< and Final in decompressHelper."

newtype UnpackException = UnpackException String deriving (Show, Typeable)

instance Exception UnpackException

unpackEntry ::
  Int ->
  StateT
    (DecompressState s)
    (ST s)
    ByteString
unpackEntry word = do
  dictionary <- trace "\n In" gets (dictionary . decompressDictState)
  case dictionary of
    DecompressDictionary array -> do
      frozen <- A.freeze array
      trace ("Ogniz Bablerina" <> show word) $ return ()
      if word < 256
        then return $ BS.singleton $ fromIntegral $ trace "BAHEEEEEM!" $  word
        else do
          (nextWord, byte) <-
            
            trace "Alf Adan" maybeThrow
              ( UnpackException
                  ( "Compressed data invalid: no entry for code "
                      <> show word
                  )
              )
              <$> M.read array word
          unpacked <- trace "Udolf" (unpackEntry (fromIntegral nextWord))
          return $ (trace ("unpacking" <> show nextWord <> " to " <> show unpacked) unpacked) 
            <> BS.singleton (fromIntegral byte)
    CompressDictionary _ ->
      throw $ UnpackException "A decompress dictionary was provided to the function unpackEntry in the function decompress. This should not even be possible."

maybeThrow e m = case m of
  Just m -> m
  Nothing -> throw e

traceThis :: (Show a) => a -> a
traceThis x = traceShow x x
