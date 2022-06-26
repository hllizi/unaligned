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
data RightOpenByteString = 
       EmptyROBs
     | ByteString :> RightOpen Word8

data LeftOpenByteString =
       EmptyLOBs
     | LeftOpen Word8 :< ByteString

deriving instance Show RightOpenByteString
deriving instance Eq RightOpenByteString

deriving instance Show LeftOpenByteString
deriving instance Eq LeftOpenByteString


makeRightOpenByteString bs used =
     if BS.null bs
        then EmptyROBs
        else BS.take (BS.length bs - 1) bs :> makeRightOpen (BS.last bs) used


makeLeftOpenByteString bs used =
     if BS.null bs
        then EmptyLOBs
        else makeLeftOpen (BS.head bs) used :< BS.tail bs

class ToByteString a where
    toByteString :: a -> ByteString

instance ToByteString RightOpenByteString where
    toByteString (bs :> RightOpen byte n) = bs `BS.append` singleton byte
    toByteString EmptyROBs = BS.empty

instance ToByteString LeftOpenByteString where
    toByteString (LeftOpen byte n :< bs) = singleton byte `BS.append` bs 
    toByteString EmptyLOBs = BS.empty

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

 -- | Take a Word16 from an unaligned right-packed Bytestring
takeWord :: LeftOpenByteString -> Int -> Maybe Word16
takeWord ((LeftOpen byte used) :< bs) numberOfBits =
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
