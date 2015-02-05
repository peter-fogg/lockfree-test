module Data.Concurrent.ScalableBag
       (
         ScalableBag
       , newBag
       , add
       , remove
       )
       where

import Control.Concurrent
import Data.Atomics
import Data.IORef
import Data.Vector as V

type ScalableBag a = Vector (IORef [a])

newBag :: IO (ScalableBag a)
newBag = do
  caps <- getNumCapabilities
  replicateM caps $ newIORef []

add :: ScalableBag a -> a -> IO ()
add bag x = atomicModifyVectorLoop bag push
  where push xs = (x:xs, Just ())

remove :: ScalableBag a -> IO (Maybe a)
remove bag = retryLoop False bag
  where
    retryLoop hasContended vec | V.null vec = if hasContended then retryLoop False bag else return Nothing
    retryLoop hasContended vec = do
      let ref = V.head vec
      tick <- readForCAS ref
      case peekTicket tick of
        [] -> retryLoop hasContended $ V.tail vec
        (x:xs) -> do
          (success, _) <- casIORef ref tick xs
          if success then return $ Just x else retryLoop True $ V.tail vec

-- Helper for the add function. Loops through the vector, trying to
-- apply the function at each index in turn until it returns a Just
-- value and the CAS succeeds.
atomicModifyVectorLoop :: ScalableBag a -> ([a] -> ([a], Maybe b)) -> IO b
atomicModifyVectorLoop bag act = getIndex >>= retryLoop . (flip V.drop $ bag)
  where retryLoop vec | V.null vec = retryLoop bag
        retryLoop vec = do
          let ref = V.head vec
          tick <- readForCAS ref
          let xs = peekTicket tick
              (new, ret) = act xs
          case ret of
            Nothing -> retryLoop $ V.tail vec
            Just val -> do
              (success, _) <- casIORef ref tick new
              if success
                then return val
                else retryLoop $ V.tail vec

-- Return the index in the vector that this thread should access.
getIndex :: IO Int
getIndex = do
  tid <- myThreadId
  (idx, _) <- threadCapability tid
  return idx
