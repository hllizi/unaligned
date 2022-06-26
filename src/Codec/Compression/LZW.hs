{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Compression.LZW where

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

compress :: CodeLength -> ByteString -> ByteString
compress codeLength bs =
  if BS.null bs
    then BS.empty
    else
      toByteString $
        compressWithMap
          (Map.empty, 255, EmptyROBs)
          (Just (fromIntegral $ BS.head bs))
          (BS.tail bs)
  where
    compressWithMap ::
      (Map (Word16, Word16) Word16, Word16, RightOpenByteString) ->
      Maybe Word16 ->
      ByteString ->
      RightOpenByteString
    compressWithMap (map, highestCode, acc) buffer bs
      | BS.null bs =
        maybe
          EmptyROBs
          ( \bufferContent ->
              pushWord
                acc
                (LeftOpen bufferContent codeLength)
          )
          buffer
      | otherwise =
        let next = fromIntegral $ BS.head bs :: Word16
         in maybe
              ( compressWithMap
                  (map, highestCode, acc)
                  (Just next)
                  (BS.tail bs)
              )
              ( \bufferContent ->
                  let extendedBuffer = (bufferContent, next)
                   in case Map.lookup extendedBuffer map of
                        Just code ->
                          compressWithMap
                            (map, highestCode, acc)
                            (Just code)
                            (BS.tail bs)
                        Nothing ->
                          let newAcc = pushBuffer extendedBuffer acc
                           in compressWithMap
                                (update (map, highestCode, newAcc) extendedBuffer)
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
