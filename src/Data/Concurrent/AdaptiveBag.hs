module Data.Concurrent.AdaptiveBag
       (
         AdaptiveBag
       , newBag
       , add
       , remove
       )
       where

import Control.Concurrent
import Control.Monad
import Data.Atomics
import Data.IORef
import Data.Vector as V

data Hybrid a = Pure [a]
              | Trans [a] (Vector (IORef [a]))
              | LockFree (Vector (IORef [a]))

type AdaptiveBag a = IORef (Hybrid a)

newBag :: IO (AdaptiveBag a)
newBag = newIORef $ Pure []

-- Push onto the bag.
add :: AdaptiveBag a -> a -> IO ()
add bag x = do
  tick <- readForCAS bag
  case peekTicket tick of
    Pure xs -> do
      (success, _) <- casIORef bag tick $ Pure (x:xs)
      unless success $ transition bag >> add bag x -- make sure this write isn't dropped
      
    Trans xs vec -> pushVec vec
    LockFree vec -> pushVec vec
  where pushVec vec =
          -- Start at the index corresponding to this thread's HEC,
          -- under the assumption that contention is least likely
          -- there, then sweep through the vector trying to find a
          -- spot to push to if that fails.
          let retryLoop v | V.null v = retryLoop vec
              retryLoop v = do
                let ref = V.head v
                tick <- readForCAS ref
                (success, _) <- casIORef ref tick (x:peekTicket tick)
                if success then return () else retryLoop $ V.tail vec
          in getIndex >>= retryLoop . (flip V.drop $ vec)

-- Attempt to pop from the bag, returning Nothing if it's empty.
remove :: AdaptiveBag a -> IO (Maybe a)
remove bag = do
  tick <- readForCAS bag
  case peekTicket tick of
    Pure [] -> return Nothing
    Pure (x:xs) -> do
      (success, _) <- casIORef bag tick $ Pure xs
      if success
        then return $ Just x
        else transition bag >> remove bag -- make sure this write isn't dropped
    Trans xs vec -> popVec vec
    LockFree vec -> popVec vec
  where popVec vec =
          -- Loop through the vector, trying to find an index that can
          -- be popped from. If the list at an index is empty, move on
          -- through the vector. If it's full, attempt to pop. Move on
          -- under contention. If the loops hits the end of the
          -- vector, there are two cases: either we've seen no
          -- contention, so the whole vector is empty, or we have, and
          -- we're at the end because we've contended each time. In
          -- the first case, return Nothing; in the second, loop
          -- again.
          let retryLoop hasContended v | V.null v = if hasContended
                                                    then retryLoop False vec
                                                    else return Nothing
              retryLoop hasContended v = do
                let ref = V.head v
                tick <- readForCAS ref
                case peekTicket tick of
                  [] -> retryLoop hasContended $ V.tail v
                  (x:xs) -> do
                    (success, _) <- casIORef ref tick xs
                    if success then return $ Just x else retryLoop True $ V.tail v
          in retryLoop False vec

transition :: AdaptiveBag a -> IO ()
transition bag = do
  val <- readIORef bag
  case val of
    Pure xs -> do
      caps <- getNumCapabilities
      vec <- V.replicateM caps $ newIORef []
      -- Fork off a thread to write into the new vector
      forkIO $ do
        let copy _ [] = return ()
            copy v _ | V.null v = copy vec xs
            copy v (x:xs) = atomicModifyIORefCAS_ (V.head v) (x:) >> copy (V.tail v) xs
        copy vec xs
        -- Copying complete, switch to the lockfree version
        atomicModifyIORefCAS_ bag (const $ LockFree vec)
      atomicModifyIORefCAS_ bag (const $ Trans xs vec)
    _ -> return () -- already in a different state; no need to transition

-- Return the index in the vector that this thread should access.
getIndex :: IO Int
getIndex = do
  tid <- myThreadId
  (idx, _) <- threadCapability tid
  return idx
