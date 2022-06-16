{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

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

data Unaligned (p :: Packing) integral = Unaligned integral Int 
  deriving (Eq)


class (Integral (EmbeddedWord a), FiniteBits (EmbeddedWord a)) => UnalignedContainer a where
  type EmbeddedWord a :: *
  usedBits :: a -> Int
  makeUnaligned :: 
            EmbeddedWord a -> 
            Int -> 
            a

-- | gives the number corresponding to a mask of size wordSize with a number of unsetBits unset at the left end of the mask.
makeMask setBits = 2 ^ setBits - 1

instance (Integral i, FiniteBits i) => UnalignedContainer (Unaligned 'RightPacked i) where
  type EmbeddedWord (Unaligned 'RightPacked i) = i
  usedBits (Unaligned _ n) = n
  makeUnaligned integral n = Unaligned unusedToZero n
   where
    unusedToZero =
      let mask = fromIntegral $ makeMask n
       in fromIntegral $ mask .&. integral

instance (Integral i, FiniteBits i) => UnalignedContainer (Unaligned 'LeftPacked i) where
  type EmbeddedWord (Unaligned 'LeftPacked i) = i
  usedBits (Unaligned _ n) = n
  makeUnaligned integral n = Unaligned unusedToZero n
   where
    unusedToZero =
      let mask = shiftL 
                    (fromIntegral $ makeMask n)
                    (finiteBitSize integral - n)
       in fromIntegral $ mask .&. integral


instance (Show i) => Show (Unaligned p i) where
  show (Unaligned x _) = show x

-- | Non-empty Bytestring with designated incomplete last byte. This way, there is no need to deal with emptiness here, as the byte to be operated upon can be obtained by simple pattern matching.
data UnalignedBytestring (p :: Packing) where
    (:>) :: ByteString -> Unaligned LeftPacked Word8 -> UnalignedBytestring LeftPacked
    (:<) :: Unaligned RightPacked Word8 -> ByteString -> UnalignedBytestring RightPacked

deriving instance Show (UnalignedBytestring p)


-- | get the last (unaligned) byte of an UnalignedBytestring LeftPacked
lastByte :: UnalignedBytestring LeftPacked -> Unaligned LeftPacked Word8
lastByte (_ :> w) = w

-- | Get the left byte of a 16 Bit word
leftByte :: Word16 -> Word8
leftByte word = fromIntegral $ shiftR word 8

-- | Get the right byte of a 16 Bit word
rightByte :: Word16 -> Word8
rightByte word = fromIntegral $ word .&. 255

combineTwoBytes :: Word8 -> Word8 -> Word16
combineTwoBytes leftByte rightByte =
  let leftByte16 = fromIntegral @_ @Word16 leftByte
      rightByte16 = fromIntegral @_ @Word16 rightByte
   in shiftL leftByte16 8 `xor` rightByte16

-- | push a right-packed unaligned 16-Bit word into an unaligned left-packed Bytestring. 
pushWord ::
  UnalignedBytestring LeftPacked ->
  Unaligned RightPacked Word16 ->
  UnalignedBytestring LeftPacked
pushWord (bs :> (Unaligned lastByte usedLeft)) (Unaligned word usedRight) =
  let unusedLeft = 8 - usedLeft
      unusedRight = 16 - usedRight
      shiftValue = unusedRight - usedLeft
      wordAdjusted =
        if shiftValue >= 0
          then shiftL word (fromIntegral shiftValue)
          else shiftR word (fromIntegral (- shiftValue))
      filledUpLastByte = lastByte `xor` leftByte wordAdjusted
      shiftedRestOfWord = shiftL word (fromIntegral (unusedLeft + unusedRight))
      resultUnused = mod (unusedLeft + unusedRight) 8
   in if usedRight - (unusedLeft + unusedRight) < 8
        then
          (bs `snoc` filledUpLastByte)
            :> Unaligned (leftByte shiftedRestOfWord) (8 - resultUnused)
        else
          (bs `snoc` filledUpLastByte `snoc` leftByte shiftedRestOfWord)
            :> Unaligned (rightByte shiftedRestOfWord) (8 - resultUnused)


