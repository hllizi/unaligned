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

type CompressState = (Map (Word16, Word16) Word16, Word16, RightOpenByteString)

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
          (Map.empty, 255, EmptyROBs)
  where
    compressWithMap ::
      Maybe Word16 ->
      ByteString ->
      State CompressState RightOpenByteString
    compressWithMap buffer bs
      | BS.null bs = do
        (map, highestCode, acc) <- get
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
        (map, highestCode, acc) <- get
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
                               update (map, highestCode, newAcc) extendedBuffer
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
      (Map (Word16, Word16) Word16, Word16, RightOpenByteString) -> (Word16, Word16) -> (Map (Word16, Word16) Word16, Word16, RightOpenByteString)
    update (map, highestCode, acc) buffer =
      if highestCode <= 2 ^ codeLength - 1
        then (insert buffer (highestCode + 1) map, highestCode + 1, acc)
        else (map, highestCode, acc)