module Main where

--import GHC.TypeLits
--import qualified MyLib (someFunc)
import Data.ByteString.Lazy as BS
import Codec.Compression.LZW
import Data.Word

repeat list 0 = []
repeat list n = list <> (Main.repeat list $ n - 1)

main :: IO ()
main = do
    let nats = [0..255] :: [Word8]
    let example = Main.repeat nats 1000
    file <- BS.readFile "./tests/usher.txt"
    file2 <- BS.readFile "/home/dlahm/tmp/2981.txt"
    let compressed = compress 14 file2
    print $ compressed 
    print $ decompress 14 compressed
    --print $ decompress 14 $ compress 14 $ BS.pack example
 -- MyLib.someFunc
