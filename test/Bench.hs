{-# LANGUAGE BangPatterns #-}

module Main
       where

import Control.Concurrent
import Control.Concurrent.Async (wait, withAsyncOn)
import Control.Monad
import Criterion.Main
import Criterion.Types
import Data.Int

import qualified Data.Concurrent.PureQueue as PQ
import qualified Data.Concurrent.Queue.MichaelScott as MS
import qualified Data.Concurrent.PureBag as PB
import qualified Data.Concurrent.ScalableBag as SB

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

pushPopN :: IO a -> (a -> Int64 -> IO ()) -> (a -> IO (Maybe Int64)) -> Int64 -> Int64 -> IO ()
pushPopN newBag push pop total batch = do
  bag <- newBag
  for_ 1 total $ \i -> do
    for_ 1 batch $ \j -> push bag j
    for_ 1 batch $ \_ -> pop bag

forkNPushPop :: IO a -> (a -> Int64 -> IO ()) -> (a -> IO (Maybe Int64)) -> Int -> Int -> IO ()
forkNPushPop newBag push pop elems splits = do
  bag <- newBag
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits (\chunk -> do
                      --  Interleave pushes and pops
                      if even chunk then void $ pop bag
                        else do
                        let offset = fromIntegral $ chunk * fromIntegral quota
                        for_ offset (offset + quota - 1) $
                          \i -> push bag i)

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
      ],
    bgroup "PureBag" [
      bench "new" $ Benchmarkable $ rep PB.newBag,
      bgroup "single-threaded" [
        bench ("push-pop-n" ++ show n) $ Benchmarkable $ rep (pushPopN PB.newBag PB.add PB.remove n b)
        | n <- sizes, b <- batches
        ],
      bgroup "multi-threaded" [
        bench ("push-pop-n" ++ show elems) $ Benchmarkable $ rep (forkNPushPop PB.newBag PB.add PB.remove elems splits)
        | elems <- parSizes
        ]
      ],
    bgroup "ScalableBag" [
      bench "new" $ Benchmarkable $ rep SB.newBag,
      bgroup "single-threaded" [
        bench ("push-pop-n" ++ show n) $ Benchmarkable $ rep (pushPopN SB.newBag SB.add SB.remove n b)
        | n <- sizes, b <- batches
        ],
      bgroup "multi-threaded" [
        bench ("push-pop-n" ++ show elems) $ Benchmarkable $ rep (forkNPushPop SB.newBag SB.add SB.remove elems splits)
        | elems <- parSizes
        ]
      ]
    ]
  where sizes = [10^e | e <- [0..4]]
        batches = [50, 500]
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
