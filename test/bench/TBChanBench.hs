{-
In-Reply-To: <4DA8E984.7010502@freegeek.org>
References: <4D993C84.8080501@freegeek.org>
	<BANLkTi=xQDTLaTKnu5ZJkSzXhrwZWXfW6g@mail.gmail.com>
	<4DA8E984.7010502@freegeek.org>
Date: Sat, 16 Apr 2011 10:57:11 -0700
Message-ID: <BANLkTimFc2LLtZ9fie1WU=tn9ns3rix7Qw@mail.gmail.com>
Subject: Re: ANN: stm-chans: Additional types of channels for STM.
From: Thomas DuBuisson <thomas.dubuisson@gmail.com>
To: wren ng thornton <wren@freegeek.org>

On Fri, Apr 15, 2011 at 5:57 PM, wren ng thornton <wren@freegeek.org> wrote:
> On 4/3/11 11:58 PM, Thomas DuBuisson wrote:
>> Wren,
>> Glad to see someone is doing a more complete packaging of STM
>> helpers and derivatives!
>>
>> I've done a little work on bounded TChans[1] (hackage "bounded-tchan"
>> package) and I think you should consider a few things:
>>
>> 1) Split the reader counter and writer counters as I've done in
>> bounded-tchan. This gives 2-5 times better performance (I
>> benchmarked using Criterion, a single reader, a single writer,
>> and tested channels with bounds of 10, 100, and 1000 elements).
>
> Do you still have those benchmarks handy? I wanted to verify the
> results and try out a few other changes, but it's not entirely
> clear to me how best to test STM (or other parallel code) with
> Criterion.

No, but I can rebuild the benchmark... see below.

Notice the many very small atomically blocks, along with a single
reader and single writer, are perfectly designed to increase
contention if you use a single counter but don't contend at all (in
the normal case) when you use two separate counters. In other words,
this benchmark is as strongly biased toward the two-counter solution
as you'll ever see.

Cheers,
Thomas
-}

-- For the copied bounded-tchan-0.2.2:Control.Concurrent.STM.BTChan.
{-# LANGUAGE BangPatterns #-}

-- This code is modified from the example Thomas gave.
module TBChanBench (main) where

import Criterion
import Criterion.Main
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBChan   as B1
import qualified Control.Concurrent.STM.TBChan2  as B2
import qualified Control.Concurrent.STM.TBMChan  as BM1
import qualified Control.Concurrent.STM.TBMChan2 as BM2
import Control.Concurrent                        (forkIO)
import Control.Monad                             (replicateM_)

-- For the copied bounded-tchan-0.2.2:Control.Concurrent.STM.BTChan.
import Control.Concurrent.STM
import Control.Monad          (when)
----------------------------------------------------------------

{-# INLINE forkReaderAndWriter #-}
forkReaderAndWriter
    :: (Int -> IO (c a))
    -> (c a -> STM b)
    -> (c a -> a -> STM ())
    -> a
    -> Int
    -> Int
    -> IO ()
forkReaderAndWriter newC readC writeC x times bound = do
    c <- newC bound
    forkIO $ replicateM_ times (atomically (writeC c x))
    replicateM_ times (atomically (readC c))

forkRW_B1 = forkReaderAndWriter B1.newTBChanIO B1.readTBChan B1.writeTBChan ()
forkRW_B2 = forkReaderAndWriter B2.newTBChanIO B2.readTBChan B2.writeTBChan ()
forkRW_B3 = forkReaderAndWriter newBTChanIO readBTChan writeBTChan ()

forkRW_BM1 =
    forkReaderAndWriter BM1.newTBMChanIO BM1.readTBMChan BM1.writeTBMChan ()
forkRW_BM2 =
    forkReaderAndWriter BM2.newTBMChanIO BM2.readTBMChan BM2.writeTBMChan ()

main :: IO ()
main = defaultMain
    [ bench "TBChan-10"     (whnfIO $ forkRW_B1 times 10)
    , bench "TBChan-100"    (whnfIO $ forkRW_B1 times 100)
    , bench "TBChan-1000"   (whnfIO $ forkRW_B1 times 1000)
    
    , bench "TBChan2-10"    (whnfIO $ forkRW_B2 times 10)
    , bench "TBChan2-100"   (whnfIO $ forkRW_B2 times 100)
    , bench "TBChan2-1000"  (whnfIO $ forkRW_B2 times 1000)
    
    , bench "BTChan-10"    (whnfIO $ forkRW_B3 times 10)
    , bench "BTChan-100"   (whnfIO $ forkRW_B3 times 100)
    , bench "BTChan-1000"  (whnfIO $ forkRW_B3 times 1000)
    {-
    , bench "TBMChan-10"    (whnfIO $ forkRW_BM1 times 10)
    , bench "TBMChan-100"   (whnfIO $ forkRW_BM1 times 100)
    , bench "TBMChan-1000"  (whnfIO $ forkRW_BM1 times 1000)
    
    , bench "TBMChan2-10"   (whnfIO $ forkRW_BM2 times 10)
    , bench "TBMChan2-100"  (whnfIO $ forkRW_BM2 times 100)
    , bench "TBMChan2-1000" (whnfIO $ forkRW_BM2 times 1000)
    -}
    ]
    where times = 100000

----------------------------------------------------------------
----- Copied from bounded-tchan-0.2.2:Control.Concurrent.STM.BTChan (not using the package itself because it doesn't compile on older GHCs due to no Applicative STM instance).

data BTChan a = BTChan
    { maxSize   :: {-# UNPACK #-} !Int
    , channel   :: (TChan a)
    , readSize  :: (TVar Int)
    , writeSize :: (TVar Int)
    }

-- This one is altered...
newBTChanIO :: Int -> IO (BTChan a)
newBTChanIO m = do
    c <- newTChanIO
    r <- newTVarIO 0
    w <- newTVarIO 0
    return (BTChan m c r w)

writeBTChan :: BTChan a -> a -> STM ()
writeBTChan (BTChan mx c rdTV wrTV) x = do
    sz <- readTVar wrTV
    if (sz >= mx)
        then do
            rsz <- readTVar rdTV
            let !newWR = sz + rsz
            when (newWR >= mx) retry
            writeTVar wrTV (newWR+1)
            writeTVar rdTV 0
            writeTChan c x
        else do
            writeTVar wrTV (sz + 1)
            writeTChan c x

readBTChan :: BTChan a -> STM a
readBTChan (BTChan _ c rdTV wrTV) = do
    x <- readTChan c
    sz <- readTVar rdTV
    let !sz' = sz - 1
    writeTVar rdTV sz'
    return x

----------------------------------------------------------------
{- On the most recent run, with -fthreaded and times=100000 :

* TBChan-10:     239.1154 ms +/- 9.015134 ms
* TBChan-100:    187.4810 ms +/- 8.778359 ms
* TBChan-1000:   186.2068 ms +/- 9.278518 ms

* TBChan2-10:    241.3167 ms +/- 9.718021 ms
* TBChan2-100:   187.8761 ms +/- 5.652408 ms
* TBChan2-1000:  188.0624 ms +/- 6.523334 ms

* BTChan-10:     236.4738 ms +/- 10.24673 ms
* BTChan-100:    184.4301 ms +/- 8.158448 ms
* BTChan-1000:   185.0646 ms +/- 8.185100 ms


* TBMChan-10:    274.8709 ms +/- 11.01923 ms
* TBMChan-100:   216.6476 ms +/- 6.443122 ms
* TBMChan-1000:  213.6817 ms +/- 5.679679 ms

* TBMChan2-10:   275.2378 ms +/- 10.04612 ms
* TBMChan2-100:  212.5276 ms +/- 4.850145 ms
* TBMChan2-1000: 210.9640 ms +/- 5.708563 ms

Which suggests that they're about even. I wonder what's different?
-}
----------------------------------------------------------------
----------------------------------------------------------- fin.
