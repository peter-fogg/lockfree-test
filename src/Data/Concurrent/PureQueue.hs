module Data.Concurrent.PureQueue
       (
         PureQueue
       , newQ
       , nullQ
       , pushL
       , tryPopR
       )
       where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.Atomics
import Data.IORef
import Data.Sequence as S

type PureQueue a = IORef (Seq a)

newQ :: IO (PureQueue a)
newQ = newIORef empty

nullQ :: PureQueue a -> IO Bool
nullQ queue = S.null <$> readIORef queue

pushL :: PureQueue a -> a -> IO ()
pushL queue x = do
  tick <- readForCAS queue
  let s = peekTicket tick
      new = x <| s
      retryLoop t = do
        (success, t') <- casIORef queue t new
        unless success $ retryLoop t'
  retryLoop tick

tryPopR :: PureQueue a -> IO (Maybe a)
tryPopR queue = do
  tick <- readForCAS queue
  case viewr $ peekTicket tick of
    EmptyR -> return Nothing
    s :> x -> let retryLoop t = do
                    (success, t') <- casIORef queue t s
                    if success then return (Just x) else retryLoop t'
              in retryLoop tick
