{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unaligned(
   RightOpen(..),
   LeftOpen(..),
   LeftOpenByteString(Final, (:<)),
   RightOpenByteString(..),
   lobsContent,
   lobsLengthOfNextWord,
   lobsUsedBitsInFirstByte,
   makeLeftOpen,
   makeRightOpen,
   makeLeftOpenByteString,
   makeRightOpenByteString,
   mergeWord,
   takeWord,
   leftAlign,
   combineTwoBytes,
   leftByte,
   rightByte,
   makeMask
)
where

import qualified Data.BitString as BitS
import Data.Bits
import Data.ByteString.Lazy as BS
import Data.Proxy
import Data.Maybe
import Data.TypeNums
import Data.Word
import Debug.Trace
import GHC.Exts

data LeftOpen integral = LeftOpen integral Int
  deriving (Show, Eq)

makeLeftOpen integral n = LeftOpen unusedToZero n
  where
    unusedToZero =
      let mask = fromIntegral $ makeMask n
       in fromIntegral $ mask .&. integral

data RightOpen integral = RightOpen
  { rightOpenContent :: !integral,
    rightOpenUsedBits :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq)

makeRightOpen integral n = RightOpen unusedToZero n
  where
    unusedToZero =
      let mask =
            shiftL
              (fromIntegral $ makeMask n)
              (finiteBitSize integral - n)
       in fromIntegral $ mask .&. integral

-- | Gives the number corresponding to a mask of size wordSize with a number of unsetBits unset at the left end of the mask.
makeMask setBits = 2 ^ setBits - 1

--instance (Show i) => Show (LeftOpen i) where
--    show (LeftOpen x _) = show x
--
--instance (Show i) => Show (RightOpen i) where
--    show (RightOpen x _) = show x
--

-- | Non-empty Bytestring with (if non-empty) designated incomplete first or last byte.
data RightOpenByteString
  = EmptyROBs
  | ByteString :> RightOpen Word8

data LeftOpenByteString = LeftOpenByteString
  { lobsContent :: ByteString,
    lobsUsedBitsInFirstByte :: {-# UNPACK #-} !Int,
    lobsLengthOfNextWord :: {-# UNPACK #-} !Int
  }
deriving instance Show LeftOpenByteString

deriving instance Eq LeftOpenByteString

makeLeftOpenByteString :: ByteString 
                       -> Int
                       -> Int
                       -> LeftOpenByteString
makeLeftOpenByteString content usedInFirstByte lengthOfNextWord 
     | lengthOfNextWord > 16 = error "Word length cannot exceed 16"
     | usedInFirstByte == 0 = error "Bytes with no used bits are not allowed"
     | otherwise = LeftOpenByteString content usedInFirstByte lengthOfNextWord

deriving instance Show RightOpenByteString

deriving instance Eq RightOpenByteString

makeRightOpenByteString bs used =
  if BS.null bs
    then EmptyROBs
    else BS.take (BS.length bs - 1) bs :> makeRightOpen (BS.last bs) used

-- | Get the left byte of a 16 Bit word
leftByte :: Word16 -> Word8
leftByte word = fromIntegral $ shiftR word 8

-- | Get the right byte of a 16 Bit word
rightByte :: Word16 -> Word8
rightByte word = fromIntegral $ word .&. 255

-- | Combine to bytes into a Word16
combineTwoBytes :: Word8 -> Word8 -> Word16
combineTwoBytes leftByte rightByte =
  let leftByte16 = fromIntegral @_ @Word16 leftByte
      rightByte16 = fromIntegral @_ @Word16 rightByte
   in shiftL leftByte16 8 `xor` rightByte16

-- | Place as many of the highest-valued used bits of the second argument in the unused bits of the first and shift the remaining used bits of the second argument to its left edge. Return the results of these operation as a pair of a ByteString of complete byte and and uncomplecte RightOpen byte.
mergeWord :: RightOpen Word8 -> LeftOpen Word16 -> (ByteString, RightOpen Word8)
mergeWord target@(RightOpen byte usedLeft) source@(LeftOpen word usedRight) =
  let unusedLeft = 8 - usedLeft -- number of bits unused at the end of the last byte
      unusedRight = 16 - usedRight -- number of bits unused at the beginning of the word to be pushed
      shiftValue = unusedRight - usedLeft -- how much (and in which direction) we need to shift to align the used bits of left and right.
      wordAdjusted =
        shift word (fromIntegral shiftValue)
      filledUpByte = byte `xor` leftByte wordAdjusted
      shiftedRestOfWord = shiftL word (fromIntegral (unusedLeft + unusedRight))
      byteCompleted = unusedLeft <= usedRight
      resultUnused = mod (unusedLeft + unusedRight) 8
      resultUsed =
        let used = (8 - resultUnused)
         in if used == 8 then 0 else used
   in if not byteCompleted
        then (empty, RightOpen filledUpByte resultUsed)
        else
          if usedRight + usedLeft < 16
            then
              ( singleton filledUpByte,
                RightOpen (leftByte shiftedRestOfWord) resultUsed
              )
            else
              ( filledUpByte `cons` singleton (leftByte shiftedRestOfWord),
                RightOpen (rightByte shiftedRestOfWord) resultUsed
              )

-- | maybeHead for ByteStrings
maybeHead :: ByteString -> Maybe Word8
maybeHead byteString =
  if BS.null byteString
    then Nothing
    else Just $ BS.head byteString

-- | Take a Word16 from an unaligned Bytestring. Expects wordLegnth to be <= 16.
takeWord :: LeftOpenByteString -> (Maybe Word16, LeftOpenByteString)
takeWord input@(LeftOpenByteString sourceByteString usedBitsInFirstByte wordLength)
  | wordLength < 1 = (Nothing, input)
  | otherwise =
    let numberOfBitsNotFromFirstByte = wordLength - usedBitsInFirstByte
        numberOfNeededBytes = ceiling $ fromIntegral numberOfBitsNotFromFirstByte / 8 + 1
        (maybeCurrent, rest) = chopNBytes numberOfNeededBytes
     in fromMaybe (Nothing, input) $ do
          current <- maybeCurrent
          ( let firstByte = fromIntegral (BS.head current) :: Word16
                adjustedFirstByte = shift firstByte numberOfBitsNotFromFirstByte
                (word, LeftOpen byte n) =
                     fitIn
                        numberOfBitsNotFromFirstByte
                        (BS.tail current)
                        adjustedFirstByte
                (usedBits, stringRest) =
                      if n == 0 then (8, rest) else (n, byte `cons` rest)
                 in pure (Just word, LeftOpenByteString stringRest usedBits wordLength)
              )
  where
    chopNBytes :: Int -> (Maybe ByteString, ByteString)
    chopNBytes n = 
      if lengthNotBelow n sourceByteString
        then
         let nTypeAdjusted = fromIntegral n
          in
            ( Just $ BS.take nTypeAdjusted sourceByteString,
              BS.drop nTypeAdjusted sourceByteString
            )
        else (Nothing, sourceByteString)

lengthNotBelow :: Int -> ByteString -> Bool
lengthNotBelow 0 _ = True
lengthNotBelow n bs = not (BS.null bs) && lengthNotBelow (n - 1) (BS.tail bs)

fitIn :: Int -> ByteString -> Word16 -> (Word16, LeftOpen Word8)
fitIn bitsToFitIn bs targetWord
      | bitsToFitIn <= 0 = (targetWord, LeftOpen 0 0)
      | bitsToFitIn > 8 =
        let remainingBits = (bitsToFitIn - 8)
            adjustedFirstByte = shift (fromIntegral $ BS.head bs) remainingBits
         in fitIn
              remainingBits
              (BS.tail bs)
              $ targetWord `xor` adjustedFirstByte
      | otherwise =
        let bitsRemainingInInput = (8 - bitsToFitIn)
            remainder = makeMask bitsRemainingInInput .&. fromIntegral (BS.head bs)
         in ( targetWord
                `xor` shiftR
                  (fromIntegral $ BS.head bs)
                  bitsRemainingInInput,
              LeftOpen remainder bitsRemainingInInput
            )

pattern w :< rest <- (takeWord -> (Just w, rest))

pattern Final bs <- (takeWord -> (Nothing, bs))

-- | Shift the used bits in a left open ByteString to the left edge.
leftAlign :: LeftOpenByteString -> ByteString
leftAlign (LeftOpenByteString bs n _)
  | BS.null bs = BS.empty
  | BS.null . BS.tail $ bs = singleton $ shift (BS.head bs) (8 - n)
  | otherwise =
    let unusedBits = (8 - n)
        headAligned = shift (BS.head bs) unusedBits
        remainderElement = shiftR (BS.head . BS.tail $ bs) n
        newHead = headAligned `xor` remainderElement
        adjustedRest = leftAlign $ LeftOpenByteString (BS.tail bs) n 0
     in newHead `cons` adjustedRest



