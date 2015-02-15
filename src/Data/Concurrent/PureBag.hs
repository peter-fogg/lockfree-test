{-# LANGUAGE BangPatterns #-}
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
add bag x = do
  tick <- readForCAS bag
  let !a = peekTicket tick
  (success, _) <- casIORef bag tick (x:a)
  if success then return () else add bag x

remove :: PureBag a -> IO (Maybe a)
remove bag = do
  tick <- readForCAS bag
  case peekTicket tick of
   [] -> return Nothing
   (x:xs) -> do
     (success, _) <- casIORef bag tick xs
     if success then return $! Just x else remove bag

-- remove bag = atomicModifyIORefCAS bag pop
--   where pop [] = ([], Nothing)
--         pop (x:xs) = (xs, Just x)
