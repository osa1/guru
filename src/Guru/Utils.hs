module Guru.Utils where

import Data.IORef

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef' ref (\a -> (f a, ()))
