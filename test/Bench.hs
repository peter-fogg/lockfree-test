{-# LANGUAGE BangPatterns #-}

module Main
       where

import Control.Concurrent
import Control.Concurrent.Async (wait, withAsyncOn)
import Criterion.Main
import Criterion.Types
import Data.Int

import qualified Data.Concurrent.PureQueue as PQ
import qualified Data.Concurrent.Queue.MichaelScott as MS

fillN :: IO a -> (a -> Int64 -> IO ()) -> Int64 -> IO ()
fillN newQ insert num = do
  q <- newQ
  for_ 1 num $ \i -> insert q i

forkNFill :: IO a -> (a -> Int64 -> IO ()) -> Int -> Int -> IO ()
forkNFill newQ insert elems splits = do
  q <- newQ
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits (\chunk -> do
                      let offset = fromIntegral $ chunk * fromIntegral quota
                      for_ offset (offset + quota - 1) $
                        \i -> insert q i)

main :: IO ()
main = do
  splits <- getNumCapabilities
  putStrLn $ "using " ++ show splits ++ " capabilities"
  defaultMain [
    bgroup "PureQueue" [
       bench "new" $ Benchmarkable $ rep PQ.newQ,
       bgroup "single-threaded" [
          bench ("push-"++show n) $ Benchmarkable $ rep (fillN PQ.newQ PQ.pushL n)
          | n <- sizes],
       bgroup "multi-threaded" [
          bench ("push-"++show elems) $ Benchmarkable $ rep (forkNFill PQ.newQ PQ.pushL elems splits)
          | elems <- parSizes]
       ],
    bgroup "LinkedQueue" [
      bench "new" $ Benchmarkable $ rep MS.newQ,
       bgroup "single-threaded" [
          bench ("push-"++show n) $ Benchmarkable $ rep (fillN MS.newQ MS.pushL n)
          | n <- sizes],
       bgroup "multi-threaded" [
          bench ("push-"++show elems) $ Benchmarkable $ rep (forkNFill MS.newQ MS.pushL elems splits)
          | elems <- parSizes]
      ]
    ]
  where sizes = [10^e | e <- [0..4]]
        parSizes = [ 10000, 100000, 500000 ]

for_ :: Monad m => Int64 -> Int64 -> (Int64 -> m a) -> m ()
for_ start end _ | start > end = error "start greater than end"
for_ start end fn = loop start
  where loop !i | i > end = return ()
                | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}

-- | Run N copies of an IO action in parallel. Pass in a number from
-- 0..N-1, letting the worker know which it is.
forkJoin :: Int -> (Int -> IO ()) -> IO ()
forkJoin num act = loop2 num []
  where
    -- VERSION 2: The less safe version:
    loop2 0 ls = mapM_ takeMVar ls
    loop2 n ls = do mv <- newEmptyMVar
                    _ <- forkOn (n-1) (do act (n-1); putMVar mv ())
                    loop2 (n-1) (mv:ls)

rep :: Monad m => m a -> Int64 -> m ()
rep m n = for_ 1 n $ \_ -> m
