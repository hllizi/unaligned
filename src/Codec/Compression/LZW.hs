{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Compression.LZW where

import Control.Monad.State
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

data CompressState = CompressState {
                dictState :: DictionaryState
            ,   acc :: RightOpenByteString
            }

data DictionaryState = DictionaryState
            {
                dictionary :: Map (Word16, Word16) Word16
            ,   nextCode :: Word16
            }

compress :: CodeLength -> ByteString -> ByteString
compress codeLength bs =
  if BS.null bs
    then BS.empty
    else
      toByteString $
        evalState
          ( compressWithMap
              (Just (fromIntegral $ BS.head bs))
              (BS.tail bs)
          )
          (CompressState    
            (DictionaryState Map.empty 256) 
            EmptyROBs)
  where
    compressWithMap ::
      Maybe Word16 ->
      ByteString ->
      State CompressState RightOpenByteString
    compressWithMap buffer bs
      | BS.null bs = do
        CompressState _ acc <- get
        return $
          maybe
            EmptyROBs
            ( \bufferContent ->
                pushWord
                  acc
                  (LeftOpen bufferContent codeLength)
            )
            buffer
      | otherwise = do
            CompressState dictState@(DictionaryState map nextCode) acc <- get
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
                          let newAcc = pushBuffer extendedBuffer acc
                              newState = 
                               let updatedDictionary =
                                    update dictState extendedBuffer
                                in CompressState 
                                    updatedDictionary
                                    newAcc
                           in do
                                put newState
                                compressWithMap
                                  Nothing
                                  (BS.tail bs)
              )
              buffer
    pushBuffer buffer =
      pushHelper (snd buffer)
        . pushHelper (fst buffer)
    pushHelper = \word -> flip pushWord (LeftOpen word codeLength)
    update ::
      DictionaryState -> (Word16, Word16) -> DictionaryState
    update dictState@(DictionaryState map nextCode) buffer =
      if nextCode <= 2 ^ codeLength - 1
        then DictionaryState 
                (insert buffer nextCode map) 
                (nextCode + 1)
        else dictState
