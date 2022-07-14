module Nuko.Driver (compile) where

import Relude (ByteString, IO, Applicative (pure))

compile :: ByteString -> IO ()
compile _ = pure ()