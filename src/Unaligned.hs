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

newtype Unaligned (p :: Packing) integral (n :: Nat) = Unaligned integral
  deriving (Eq)

make :: forall p i n. (IsPacking p ~ 'True, Integral i, KnownNat n) => i -> Unaligned p i n
make = Unaligned . unusedToZero
    where
        unusedToZero i =
            let mask = undefined
            in mask

class UnalignedContainer a where
  unusedBits :: a -> Integer

instance (KnownNat n, Integral i) => UnalignedContainer (Unaligned p i n) where
  unusedBits _ = natVal (Proxy @n)

instance (Show i) => Show (Unaligned p i n) where
  show (Unaligned x) = show x

-- Non-empty Bytestring with designated incomplete last byte
data UnalignedBytestring n = ByteString :> Unaligned LeftPacked Word8 n
  deriving (Show)

lastByte :: UnalignedBytestring n -> Unaligned LeftPacked Word8 n
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
  forall m n k.
  (KnownNat m, KnownNat n, KnownNat k, k ~ Mod (m + n) 8) =>
  UnalignedBytestring m ->
  Unaligned RightPacked Word16 n ->
  UnalignedBytestring k
pushWord (bs :> (Unaligned lastByte)) (Unaligned word) =
  let unusedLeft = natVal (Proxy @m)
      usedLeft = 8 - unusedLeft
      unusedRight = natVal (Proxy @n)
      usedRight = 16 - unusedRight
      shiftValue = unusedRight - usedLeft
      wordAdjusted =
        if shiftValue >= 0
          then shiftL word (fromIntegral shiftValue)
          else shiftR word (fromIntegral (- shiftValue))
      filledUpLastByte = lastByte `xor` leftByte wordAdjusted
      shiftedRestOfWord = shiftL word (fromIntegral (unusedLeft + unusedRight))
   in if usedRight - (unusedLeft + unusedRight) < 8
        then
          (bs `snoc` filledUpLastByte)
            :> Unaligned (leftByte shiftedRestOfWord)
        else
          (bs `snoc` filledUpLastByte `snoc` leftByte shiftedRestOfWord)
            :> Unaligned (rightByte shiftedRestOfWord)
