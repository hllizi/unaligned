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

module Unaligned where

import Data.Bits
import qualified Data.BitString as BitS
import Data.ByteString.Lazy as BS
import Data.Proxy
import Data.TypeNums
import Data.Word
import GHC.Exts
import Debug.Trace

type Bitcount = Word

data LeftOpen integral = LeftOpen integral Int
  deriving (Show, Eq)

makeLeftOpen integral n = LeftOpen unusedToZero n
  where
    unusedToZero =
      let mask = fromIntegral $ makeMask n
       in fromIntegral $ mask .&. integral

data RightOpen integral = RightOpen integral Int
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
    lobsUsedBitsInFirstByte :: Int,
    lobsLengthOfNextWord :: Int
  }

deriving instance Show RightOpenByteString

deriving instance Eq RightOpenByteString

deriving instance Show LeftOpenByteString

deriving instance Eq LeftOpenByteString

makeRightOpenByteString bs used =
  if BS.null bs
    then EmptyROBs
    else BS.take (BS.length bs - 1) bs :> makeRightOpen (BS.last bs) used

class ToByteString a where
  toByteString :: a -> ByteString

instance ToByteString RightOpenByteString where
  toByteString (bs :> RightOpen byte n) = bs `BS.append` singleton byte
  toByteString EmptyROBs = BS.empty

instance ToByteString (ByteString, RightOpen Word8) where
  toByteString (bs, RightOpen word _) = bs `snoc` word

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
  let unusedLeft = 8 - trace (show target) usedLeft -- number of bits unused at the end of the last byte
      unusedRight = 16 - trace (show source) usedRight -- number of bits unused at the beginning of the word to be pushed
      shiftValue = unusedRight - usedLeft -- how much (and in which direction) we need to shift to align the used bits of left and right.
      wordAdjusted =
          shift word (fromIntegral shiftValue)
      filledUpByte = byte `xor` leftByte wordAdjusted
      shiftedRestOfWord = shiftL word (fromIntegral (unusedLeft + unusedRight))
      byteCompleted = unusedLeft <= usedRight
      resultUnused = mod (unusedLeft + unusedRight) 8
      resultUsed = let used = (8 - resultUnused)
                    in if used == 8 then 0 else used
   in if not byteCompleted
        then (empty, RightOpen filledUpByte resultUsed)
        else
          if usedRight + usedLeft < 16 
            then
              ( singleton filledUpByte,
                RightOpen (leftByte shiftedRestOfWord) $ trace ("Used: " <> show resultUsed) resultUsed
              )
            else
              ( filledUpByte `cons` singleton (leftByte shiftedRestOfWord),
                RightOpen (rightByte shiftedRestOfWord) $ trace ("Used: " <> show resultUsed) resultUsed
              )

-- | Push a right-packed unaligned 16-Bit word into an unaligned left-packed Bytestring.
pushWord ::
  RightOpenByteString ->
  LeftOpen Word16 ->
  RightOpenByteString
pushWord EmptyROBs (LeftOpen word usedRight) =
  let unusedRight = (16 - usedRight)
      wordAdjusted = shiftL word unusedRight
   in if usedRight <= 8
        then BS.empty :> RightOpen (leftByte wordAdjusted) usedRight
        else
          (singleton . leftByte $ wordAdjusted)
            :> RightOpen (rightByte wordAdjusted) (mod usedRight 8)
pushWord (bs :> (RightOpen lastByte usedLeft)) (LeftOpen word usedRight) =
  let unusedLeft = 8 - usedLeft -- number of bits unused at the end of the last byte
      unusedRight = 16 - usedRight -- number of bits unused at the beginning of the word to be pushed
      shiftValue = unusedRight - usedLeft -- how much (and in which direction) we need to shift to align the used bits of left and right.
      wordAdjusted =
        if shiftValue >= 0
          then shiftL word (fromIntegral shiftValue)
          else shiftR word (fromIntegral (- shiftValue))
      filledUpLastByte = lastByte `xor` leftByte wordAdjusted
      shiftedRestOfWord = shiftL word (fromIntegral (unusedLeft + unusedRight))
      resultUnused = mod (unusedLeft + unusedRight) 8
   in if usedRight - resultUnused < 8
        then
          (bs `snoc` filledUpLastByte)
            :> RightOpen (leftByte shiftedRestOfWord) (8 - resultUnused)
        else
          (bs `snoc` filledUpLastByte `snoc` leftByte shiftedRestOfWord)
            :> RightOpen (rightByte shiftedRestOfWord) (8 - resultUnused)

-- | maybeHead for ByteStrings
maybeHead :: ByteString -> Maybe Word8
maybeHead byteString =
  if BS.null byteString
    then Nothing
    else Just $ BS.head byteString

-- | Take a Word16 from an unaligned Bytestring. Expects wordLegnth to be <= 16.
takeWord :: LeftOpenByteString -> (Maybe Word16, LeftOpenByteString)
takeWord input@(LeftOpenByteString sourceByteString usedBitsInFirstByte wordLength)
  | wordLength < 1 = trace ("Boudzko: " <> (show $ BitS.bitString $ toStrict sourceByteString)) (Nothing, input)
  | otherwise =
    let numberOfBitsNotFromFirstByte = wordLength - usedBitsInFirstByte
        numberOfNeededBytes = ceiling $ fromIntegral wordLength / 8
        (maybeCurrent, rest) = chopNBytes numberOfNeededBytes
     in maybe
          (trace ("Ourpel: " <> (show $ BitS.bitString $ toStrict sourceByteString)) (Nothing, input))
          ( \current ->
              ( let firstByte = fromIntegral (BS.head current) :: Word16
                    adjustedFirstByte = shift firstByte numberOfBitsNotFromFirstByte
                    (word, LeftOpen byte n) =
                      fitIn
                        numberOfBitsNotFromFirstByte
                        (BS.tail current)
                        adjustedFirstByte
                    (usedBits, stringRest) =
                      if n == 0 then (8, rest) else (n, byte `cons` rest)
                 in (Just word, LeftOpenByteString stringRest usedBits wordLength)
              )
          )
          maybeCurrent
  where
    chopNBytes :: Int -> (Maybe ByteString, ByteString)
    chopNBytes n = do
      let nTypeAdjusted = fromIntegral n
      if BS.length sourceByteString >= nTypeAdjusted
        then
          ( Just $ BS.take nTypeAdjusted sourceByteString,
            BS.drop nTypeAdjusted sourceByteString
          )
        else (Nothing, sourceByteString)

    fitIn :: Int -> ByteString -> Word16 -> (Word16, LeftOpen Word8)
    fitIn bitsToFitIn bs targetWord
      | bitsToFitIn <= 0 = (targetWord, LeftOpen 0 0)
      | bitsToFitIn > 8 =
        let remainingBits = (8 - bitsToFitIn)
            adjustedFirstByte = shift (fromIntegral $ BS.head bs) remainingBits
         in fitIn
              remainingBits
              (BS.tail bs)
              $ targetWord `xor` adjustedFirstByte
      | otherwise =
        let remainingBits = (8 - bitsToFitIn)
            remainder = makeMask remainingBits .&. fromIntegral (BS.head bs)
         in ( targetWord
                `xor` shiftR
                  (fromIntegral $ BS.head bs)
                  remainingBits,
              LeftOpen remainder remainingBits
            )

takeWordWrapper = (\taken -> trace ("taken word: " <> show taken) taken) . takeWord
pattern w :< rest <- (takeWordWrapper -> (Just w, rest))

pattern Final bs <- (takeWordWrapper -> (Nothing, bs))

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

-- Helpers

-- | Determine the number of Bytes needed to contain a number of bits.
minBytes :: Int -> Int
minBytes numberOfBits =
  numberOfBits `div` 8 + fragmentCorrection
  where
    fragmentCorrection = if numberOfBits `mod` 8 == 0 then 0 else 1
