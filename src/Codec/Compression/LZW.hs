{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}


module Codec.Compression.LZW where

import Control.Exception
import Control.Monad.ST.Lazy
import Control.Monad.State.Lazy
import GHC.Generics
import Control.Lens
import Data.Generics.Labels
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
  | DecompressDictionary !(M.STVector s (Maybe ByteString))

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
                                compressState <- get
                                updatedDictionary <-
                                  lift $
                                    evalStateT
                                      ( updateDictionary
                                          maxCodeLength
                                          extendedBuffer
                                      )
                                      (compressDictState compressState)
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
  CodeLength ->
  (Word16, Word16) ->
  StateT
    (DictionaryState s)
    (ST s)
    (DictionaryState s)
updateDictionary maxCodeLength pair = do
  dictState@(DictionaryState dictionary nextCode codeLength maxCode isFull) <- get
  let newState
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
  newState
  where
    updatedDictionaryMon :: StateT (DictionaryState s) (ST s) (Dictionary s)
    updatedDictionaryMon = do
      dictState@(DictionaryState dictionary nextCode codeLength maxCode isFull) <- get
      case dictState ^. #dictionary of
        CompressDictionary map -> pure $ CompressDictionary $ M.insert pair nextCode map
        DecompressDictionary array ->
          DecompressDictionary <$> do
            firstDecoded <- fromMaybe (BS.singleton $ fromIntegral $ fst pair) <$> M.read array (fromIntegral $ fst pair)
            let newEntry = firstDecoded `BS.snoc` fromIntegral (snd pair)
            M.write array (fromIntegral nextCode) $  Just newEntry
            --pure $ trace ("boonish: " <> show bamboon) array
            pure array

type DecompressState s = DictionaryState s

decompress :: CodeLength -> ByteString -> ByteString
decompress maxCodeLength compressed =
  let input = makeLeftOpenByteString compressed 8 9
   in runST $ do
        let maxNumberOfCodes = 2 ^ maxCodeLength
        prefilledVector <-
            M.generate
              maxNumberOfCodes
              ( \n ->
                  if n > 255
                    then Nothing
                    else Just $ BS.singleton $ fromIntegral n
              )
        let initState = 
              makeDictionaryState
                ( DecompressDictionary
                    prefilledVector
                )
                256
                9
                False
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
        DictionaryState
          { dictionary = ~(DecompressDictionary decompDict),
            nextCode = nextcode,
            codeLength = length
          } <-
          get
        vector <- A.unsafeFreeze decompDict
        return $ unpackEntry vector (fromIntegral w)
    decompressHelper whole@(w1 :< (w2 :< lobs)) =
      do
        dictSt@DictionaryState
          { dictionary = ~(DecompressDictionary decompDict),
            nextCode = nextcode,
            codeLength = length
          } <-
          get
        boff <-  M.read decompDict 256
        vector <- A.unsafeFreeze decompDict
        let codeUnpacked = unpackEntry vector (fromIntegral w1)
        thawed <- A.unsafeThaw vector
        newState@( DictionaryState
                     (DecompressDictionary thawed)
                     _
                     newWordLength
                     _
                     isFull
                   ) <-
          updateDictionary maxCodeLength (w1, w2)
        put newState
        compressedRest <- decompressHelper (lobs {lobsLengthOfNextWord = newWordLength})
        return $ 
          codeUnpacked
            <> BS.singleton (rightByte w2)
            <> compressedRest

    -- without the following line, hls complains about non-exhaustive pattern matches even though the use of the actually exported (non-constructor) patterns should is exhaustive.
    decompressHelper x = error $ "This should not have happened: " <> show x <> " did not match any pattern built with :< and Final in decompressHelper."

newtype UnpackException = UnpackException String deriving (Show, Typeable)

instance Exception UnpackException

unpackEntry ::
  A.Vector (Maybe ByteString) ->
  Int ->
  ByteString
unpackEntry array word = 
      maybeThrow
      ( UnpackException
          ( "Compressed data invalid: no entry for code "
              <> show word
          )
      )
      (array A.! word)

maybeThrow e m = case m of
  Just m -> m
  Nothing -> throw e

traceThis :: (Show a) => a -> a
traceThis x = traceShow x x
