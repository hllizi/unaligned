{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Unaligned where

import Data.Bits
import Data.ByteString
import Data.Proxy
import Data.TypeNums
import Data.Word

type Bitcount = Word

data Packing = RightPacked | LeftPacked

type family IsPacking a where
  IsPacking RightPacked = 'True
  IsPacking LeftPacked = 'True
  IsPacking a = 'False

data Unaligned (p :: Packing) integral = Unaligned integral Integer 
  deriving (Eq)

make :: forall p i n. (IsPacking p ~ 'True, Integral i) => 
            i -> 
            Integer -> 
            Unaligned p Integer
make = flip Unaligned . unusedToZero
  where
    unusedToZero i =
      let mask = undefined
       in mask

class UnalignedContainer a where
  unusedBits :: a -> Integer

instance Integral i => UnalignedContainer (Unaligned p i) where
  unusedBits (Unaligned _ n) = n

instance (Show i) => Show (Unaligned p i) where
  show (Unaligned x _) = show x

-- Non-empty Bytestring with designated incomplete last byte
data UnalignedBytestring = ByteString :> Unaligned LeftPacked Word8
  deriving (Show)

lastByte :: UnalignedBytestring -> Unaligned LeftPacked Word8
lastByte (_ :> w) = w

leftByte :: Word16 -> Word8
leftByte word = fromIntegral $ shiftR word 8

rightByte :: Word16 -> Word8
rightByte word = fromIntegral $ word .&. 255

combineTwoBytes :: Word8 -> Word8 -> Word16
combineTwoBytes leftByte rightByte =
  let leftByte16 = fromIntegral @_ @Word16 leftByte
      rightByte16 = fromIntegral @_ @Word16 rightByte
   in shiftL leftByte16 8 `xor` rightByte16

pushWord ::
  UnalignedBytestring ->
  Unaligned RightPacked Word16 ->
  UnalignedBytestring
pushWord (bs :> (Unaligned lastByte m)) (Unaligned word n) =
  let unusedLeft =  m
      usedLeft = 8 - unusedLeft
      unusedRight = n
      usedRight = 16 - unusedRight
      shiftValue = unusedRight - usedLeft
      wordAdjusted =
        if shiftValue >= 0
          then shiftL word (fromIntegral shiftValue)
          else shiftR word (fromIntegral (- shiftValue))
      filledUpLastByte = lastByte `xor` leftByte wordAdjusted
      shiftedRestOfWord = shiftL word (fromIntegral (unusedLeft + unusedRight))
      resultUnused = mod (m + n) 8
   in if usedRight - (unusedLeft + unusedRight) < 8
        then
          (bs `snoc` filledUpLastByte)
            :> Unaligned (leftByte shiftedRestOfWord) resultUnused
        else
          (bs `snoc` filledUpLastByte `snoc` leftByte shiftedRestOfWord)
            :> Unaligned (rightByte shiftedRestOfWord) resultUnused
