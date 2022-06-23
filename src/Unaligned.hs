{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Unaligned where

import Data.Bits
import Data.ByteString as BS
import Data.Proxy
import Data.TypeNums
import Data.Word
import GHC.Exts

type Bitcount = Word

data Orientation = LeftOpen | RightOpen

-- | To use Orientation in contexts

type family IsOrientation (t :: Orientation) :: Constraint where
    IsOrientation LeftOpen = ()
    IsOrientation RightOpen = ()

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

-- | Gives the number corresponding to a mask of size wordSize with a number of unsetBits unset at the left end of the mask.
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
        let mask =
              shiftL
                (fromIntegral $ makeMask n)
                (finiteBitSize integral - n)
         in fromIntegral $ mask .&. integral

instance (Show i) => Show (Unaligned p i) where
  show (Unaligned x _) = show x

-- | Non-empty Bytestring with designated incomplete first or last byte. This way, there is no need to deal with emptiness here, as the byte to be operated upon can be obtained by simple pattern matching.
data UnalignedBytestring (p :: Orientation) where
  (:>) :: ByteString -> Unaligned RightOpen Word8 -> UnalignedBytestring RightOpen
  (:<) :: Unaligned LeftOpen Word8 -> ByteString -> UnalignedBytestring LeftOpen

deriving instance Show (UnalignedBytestring p)
deriving instance Eq (UnalignedBytestring p)

type family ByteStringOrientation a where
    ByteStringOrientation (UnalignedBytestring o) = o
-- | Provide a common interface for LeftOpen and RightOpen ByteStrings
class UnalignedByteStringInterface i where
-- | Smart Constructor for UnalignedBytestring 
    makeUnalignedByteString :: 
           ByteString 
        -> Int 
        -> Maybe (UnalignedBytestring (ByteStringOrientation i)) 
-- | UnalignedBytestring to ByteString
    toString :: UnalignedBytestring (ByteStringOrientation i) -> ByteString


instance UnalignedByteStringInterface (UnalignedBytestring 'RightOpen)  where
    makeUnalignedByteString bs used =
     if BS.null bs 
        then Nothing 
        else    Just 
              $ BS.take (BS.length bs - 1) bs :> makeUnaligned (BS.last bs) used
    toString (Unaligned byte n :< bs) = BS.singleton byte `append` bs


instance UnalignedByteStringInterface (UnalignedBytestring 'LeftOpen)  where
    makeUnalignedByteString bs used =
     if BS.null bs 
        then Nothing 
        else    Just 
              $ makeUnaligned (BS.head bs) used :< BS.tail bs 
    toString (bs :> Unaligned byte n) = bs `append` BS.singleton byte


-- | Get the last (unaligned) byte of an UnalignedBytestring RightOpen
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

-- | Push a right-packed unaligned 16-Bit word into an unaligned left-packed Bytestring.
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

-- | Take a Word16 from an unaligned right-packed Bytestring
takeWord :: UnalignedBytestring 'LeftOpen -> Int -> Maybe Word16
takeWord ((Unaligned byte used) :< bs) numberOfBits =
    let numberOfBitsNotFromFirstByte = numberOfBits - used
        word = fromIntegral byte :: Word16
        adjustedFirstByte =
          if numberOfBitsNotFromFirstByte < 0
            then shiftR word (- numberOfBitsNotFromFirstByte)
            else shiftL word numberOfBitsNotFromFirstByte
     in 
         fitIn numberOfBitsNotFromFirstByte bs (fromIntegral adjustedFirstByte)
  where
    additionalBytes :: Maybe ByteString
    additionalBytes =
      let numberOfNeededBytes = minBytes (numberOfBits - used)
       in if BS.length bs >= numberOfNeededBytes
            then Just (BS.take numberOfNeededBytes bs)
            else Nothing
    fitIn :: Int -> ByteString -> Word16 -> Maybe Word16
    fitIn numberOfBits bs word
      | numberOfBits == 0 = Just word
      | numberOfBits > 8 =
        if BS.length bs >= minBytes numberOfBits
          then
            fitIn
               (numberOfBits - 8)
               (BS.tail bs)
            $  word `xor` shiftL (fromIntegral $ BS.head bs) (numberOfBits - 8)
          else Nothing
      | otherwise =
        Just $ word `xor` shiftR (fromIntegral $ BS.head bs) (8 - numberOfBits)


-- Helpers


-- | Determine the number of Bytes needed to contain a number of bits.
minBytes :: Int -> Int
minBytes numberOfBits =
  numberOfBits `div` 8 + fragmentCorrection
  where
    fragmentCorrection = if numberOfBits `mod` 8 == 0 then 0 else 1
