module Codec.Compression.LZW.Conduit where

import Conduit
import qualified Data.ByteString.Lazy as BS
import Codec.Compression.LZW


transformer :: (Monad m) => (a -> a) -> ConduitT a a m ()
transformer transform = do
                    loop
             where loop = do
                            input <- await 
                            let maybeCompressed = 
                                            transform
                                        <$> input
                            maybe loop yield maybeCompressed

compress :: (Monad m) => CodeLength -> ConduitT BS.ByteString BS.ByteString m ()
compress codeLength = transformer $ Codec.Compression.LZW.compress codeLength
                

decompress :: (Monad m) => CodeLength -> ConduitT BS.ByteString BS.ByteString m ()
decompress codeLength = transformer $ Codec.Compression.LZW.decompress codeLength 




