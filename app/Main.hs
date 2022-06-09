{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Bits
import Data.ByteString
import Data.Proxy
import Data.TypeNums
import Data.Word
import qualified MyLib (someFunc)

type Bitcount = Word

newtype Incomplete integral (n :: Nat) = Incomplete integral

class IncompleteContainer a where
  unusedBits :: a -> Integer

instance (KnownNat n, Integral i) => IncompleteContainer (Incomplete i n) where
  unusedBits _ = natVal (Proxy @n)

-- Non-empty Bytestring with designated last byte
data IncompleteBytestring n = ByteString :> Incomplete Word8 n

lastByte :: IncompleteBytestring n -> Incomplete Word8 n
lastByte (_ :> w) = w

pushWord ::
  forall m n k.
  (KnownNat m, KnownNat n, KnownNat k) =>
  IncompleteBytestring m ->
  Incomplete Word16 n ->
  IncompleteBytestring k
pushWord bs (Incomplete word) =
  let usedLeft = 8 - natVal (Proxy @m)
      unusedRight = natVal (Proxy @n)
      shiftValue = usedLeft - unusedRight
      wrdAdjusted =
        if shiftValue <= 0
          then shiftR word (fromIntegral (- shiftValue))
          else shiftL word (fromIntegral shiftValue)
   in undefined

main :: IO ()
main = do
  Prelude.putStrLn "Hello, Haskell!"
  MyLib.someFunc
