{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Unaligned where

import Data.Bits
import Data.ByteString
import Data.Proxy
import Data.TypeNums
import Data.Word
import GHC.Exts

type Bitcount = Word

data Packing = RightPacked | LeftPacked


type family IsPacking a where
  IsPacking RightPacked = 'True
  IsPacking LeftPacked = 'True
  IsPacking a = 'False

data Unaligned (p :: Packing) integral = Unaligned integral Integer 
  deriving (Eq)


class UnalignedContainer a where
  unusedBits :: a -> Integer
  make :: forall i n. (Integral i, FiniteBits i) => 
            i -> 
            Integer -> 
            a
makeMask wordSize setBits = 2 ^ (wordSize - fromIntegral setBits) - 1

instance (Integral i, FiniteBits i) => UnalignedContainer (Unaligned 'RightPacked i) where
  unusedBits (Unaligned _ n) = n
  make integral n = Unaligned unusedToZero n
   where
    unusedToZero =
      let mask = fromIntegral $ makeMask (finiteBitSize integral) n
       in fromIntegral $ mask .&. integral

instance (Integral i, FiniteBits i) => UnalignedContainer (Unaligned 'LeftPacked i) where
  unusedBits (Unaligned _ n) = n
  make integral n = Unaligned unusedToZero n
   where
    unusedToZero =
      let mask = shiftL 
                    (fromIntegral $ makeMask (finiteBitSize integral) n)
                    (fromIntegral n)
       in fromIntegral $ mask .&. integral


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

-- | push a right-packed unaligned 16-Bit word into an unaligned left-packed Bytestring. 
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
