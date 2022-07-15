module Main (main) where

import Relude.Lifted.Terminal ( putStrLn )
import Relude.Monad (MonadIO)

main :: MonadIO m => m ()
main = putStrLn "Oh no!"