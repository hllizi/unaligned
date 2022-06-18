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

data Orientation = LeftOpen | RightOpen


data Unaligned (p :: Orientation) integral = Unaligned integral Int 
  deriving (Eq)

-- | Obtain the type of integral used from an Unaligned object.
type family EmbeddedWord a where
    EmbeddedWord (Unaligned 'LeftOpen i) = i
    EmbeddedWord (Unaligned 'RightOpen i) = i

-- | This class requires the implementation of a method to obtain the number of bits that are in use in the embedded integral and a smart constructor that ascertain that all unused bits are set to zero.
class (Integral (EmbeddedWord a), FiniteBits (EmbeddedWord a)) => UnalignedContainer a where
  usedBits :: a -> Int
  makeUnaligned :: 
            EmbeddedWord a -> 
            Int -> 
            a

-- | gives the number corresponding to a mask of size wordSize with a number of unsetBits unset at the left end of the mask.
makeMask setBits = 2 ^ setBits - 1

instance (Integral i, FiniteBits i) => UnalignedContainer (Unaligned 'LeftOpen i) where
  usedBits (Unaligned _ n) = n
  makeUnaligned integral n = Unaligned unusedToZero n
   where
    unusedToZero =
      let mask = fromIntegral $ makeMask n
       in fromIntegral $ mask .&. integral

instance (Integral i, FiniteBits i) => UnalignedContainer (Unaligned 'RightOpen i) where
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

-- | Non-empty Bytestring with designated incomplete first or last byte. This way, there is no need to deal with emptiness here, as the byte to be operated upon can be obtained by simple pattern matching.
data UnalignedBytestring (p :: Orientation) where
    (:>) :: ByteString -> Unaligned RightOpen Word8 -> UnalignedBytestring RightOpen
    (:<) :: Unaligned LeftOpen Word16 -> ByteString -> UnalignedBytestring LeftOpen

deriving instance Show (UnalignedBytestring p)


-- | get the last (unaligned) byte of an UnalignedBytestring RightOpen
lastByte :: UnalignedBytestring RightOpen -> Unaligned RightOpen Word8
lastByte (_ :> w) = w

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

-- | push a right-packed unaligned 16-Bit word into an unaligned left-packed Bytestring. 
pushWord ::
  UnalignedBytestring RightOpen ->
  Unaligned LeftOpen Word16 ->
  UnalignedBytestring RightOpen
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


-- | take a word from an unaligned right-packed Bytestring
--

takeWord :: UnalignedBytestring 'LeftOpen -> Int -> Unaligned 'LeftOpen Word16
takeWord ((Unaligned word used) :< bs) numberOfBits = 
     let uninterestingBits = 16 - numberOfBits
      in
     if uninterestingBits >= 0 then
      let 
         adjustedWord = shiftL word (16 - used) 
         mask = makeMask numberOfBits
         maskedWord = adjustedWord .&. mask 
        in 
           Unaligned maskedWord numberOfBits
     else error "cannot extract more than 16 Bits from a Word16"
