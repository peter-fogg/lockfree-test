module Data.Concurrent.PureBag
       (
         PureBag
       , newBag
       , add
       , remove
       )
       where

import Data.Atomics
import Data.IORef

type PureBag a = IORef [a]

newBag :: IO (PureBag a)
newBag = newIORef []

add :: PureBag a -> a -> IO ()
add bag x = atomicModifyIORefCAS_ bag (x:)

remove :: PureBag a -> IO (Maybe a)
remove bag = atomicModifyIORefCAS bag pop
  where pop [] = ([], Nothing)
        pop (x:xs) = (xs, Just x)
