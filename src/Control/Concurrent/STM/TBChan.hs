{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  Control.Concurrent.STM.TBChan
-- Copyright   :  Copyright (c) 2011--2021 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC STM, DeriveDataTypeable)
--
-- A version of "Control.Concurrent.STM.TChan" where the queue is
-- bounded in length. This variant incorporates ideas from Thomas
-- M. DuBuisson's @bounded-tchan@ package in order to reduce
-- contention between readers and writers.
----------------------------------------------------------------
module Control.Concurrent.STM.TBChan
    (
    -- * The TBChan type
      TBChan()
    -- ** Creating TBChans
    , newTBChan
    , newTBChanIO
    -- I don't know how to define dupTBChan with the correct semantics
    -- ** Reading from TBChans
    , readTBChan
    , tryReadTBChan
    , peekTBChan
    , tryPeekTBChan
    -- ** Writing to TBChans
    , writeTBChan
    , tryWriteTBChan
    , unGetTBChan
    -- ** Predicates
    , isEmptyTBChan
    , isFullTBChan
    -- ** Other functionality
    , estimateFreeSlotsTBChan
    , freeSlotsTBChan
    ) where

import Prelude           hiding (reads)
import Data.Typeable     (Typeable)
import Control.Monad.STM (STM, retry)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan -- N.B., GHC only
----------------------------------------------------------------

-- | @TBChan@ is an abstract type representing a bounded FIFO
-- channel.
data TBChan a = TBChan
    {-# UNPACK #-} !(TVar Int)
    {-# UNPACK #-} !(TVar Int)
    {-# UNPACK #-} !(TChan a)
    deriving (Typeable)
-- The components are:
-- * How many free slots we /know/ we have available.
-- * How many slots have been freed up by successful reads since
--   the last time the slot count was synchronized by 'isFullTBChan'.
-- * The underlying TChan.


-- | Build and returns a new instance of @TBChan@ with the given
-- capacity. /N.B./, we do not verify the capacity is positive, but
-- if it is non-positive then 'writeTBChan' will always retry and
-- 'isFullTBChan' will always be true.
newTBChan :: Int -> STM (TBChan a)
newTBChan n = do
    slots <- newTVar n
    reads <- newTVar 0
    chan  <- newTChan
    return (TBChan slots reads chan)


-- | @IO@ version of 'newTBChan'. This is useful for creating
-- top-level @TBChan@s using 'System.IO.Unsafe.unsafePerformIO',
-- because using 'Control.Monad.STM.atomically' inside
-- 'System.IO.Unsafe.unsafePerformIO' isn't possible.
newTBChanIO :: Int -> IO (TBChan a)
newTBChanIO n = do
    slots <- newTVarIO n
    reads <- newTVarIO 0
    chan  <- newTChanIO
    return (TBChan slots reads chan)


-- | Read the next value from the @TBChan@, retrying if the channel
-- is empty.
readTBChan :: TBChan a -> STM a
readTBChan (TBChan _slots reads chan) = do
    x <- readTChan chan
    modifyTVar' reads (1 +)
    return x


-- | A version of 'readTBChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTBChan :: TBChan a -> STM (Maybe a)
tryReadTBChan (TBChan _slots reads chan) = do
    mx <- tryReadTChan chan
    case mx of
        Nothing -> return Nothing
        Just _x -> do
            modifyTVar' reads (1 +)
            return mx


-- | Get the next value from the @TBChan@ without removing it,
-- retrying if the channel is empty.
peekTBChan :: TBChan a -> STM a
peekTBChan (TBChan _slots _reads chan) =
    peekTChan chan


-- | A version of 'peekTBChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTBChan :: TBChan a -> STM (Maybe a)
tryPeekTBChan (TBChan _slots _reads chan) =
    tryPeekTChan chan


-- | Write a value to a @TBChan@, retrying if the channel is full.
writeTBChan :: TBChan a -> a -> STM ()
writeTBChan self@(TBChan slots _reads chan) x = do
    n <- estimateFreeSlotsTBChan self
    if n <= 0
        then retry
        else do
            writeTVar slots $! n - 1
            writeTChan chan x
{-
-- The above comparison is unnecessary on one of the n>0 branches
-- coming from estimateFreeSlotsTBChan. But for some reason, trying
-- to remove it can cause BlockedIndefinatelyOnSTM exceptions.

-- The above saves one @readTVar slots@ compared to:
writeTBChan self@(TBChan slots _reads chan) x = do
    b <- isFullTBChan self
    if b
        then retry
        else do
            modifyTVar' slots (subtract 1)
            writeTChan chan x
-}


-- | A version of 'writeTBChan' which does not retry. Returns @True@
-- if the value was successfully written, and @False@ otherwise.
tryWriteTBChan :: TBChan a -> a -> STM Bool
tryWriteTBChan self@(TBChan slots _reads chan) x = do
    n <- estimateFreeSlotsTBChan self
    if n <= 0
        then return False
        else do
            writeTVar slots $! n - 1
            writeTChan chan x
            return True
{-
-- The above comparison is unnecessary on one of the n>0 branches
-- coming from estimateFreeSlotsTBChan. But for some reason, trying
-- to remove it can cause BlockedIndefinatelyOnSTM exceptions.

-- The above saves one @readTVar slots@ compared to:
tryWriteTBChan self@(TBChan slots _reads chan) x = do
    b <- isFullTBChan self
    if b
        then return False
        else do
            modifyTVar' slots (subtract 1)
            writeTChan chan x
            return True
-}


-- | Put a data item back onto a channel, where it will be the next
-- item read. /N.B./, this could allow the channel to temporarily
-- become longer than the specified limit, which is necessary to
-- ensure that the item is indeed the next one read.
unGetTBChan :: TBChan a -> a -> STM ()
unGetTBChan (TBChan slots _reads chan) x = do
    modifyTVar' slots (subtract 1)
    unGetTChan chan x


-- | Returns @True@ if the supplied @TBChan@ is empty (i.e., has
-- no elements). /N.B./, a @TBChan@ can be both \"empty\" and
-- \"full\" at the same time, if the initial limit was non-positive.
isEmptyTBChan :: TBChan a -> STM Bool
isEmptyTBChan (TBChan _slots _reads chan) =
    isEmptyTChan chan


-- | Returns @True@ if the supplied @TBChan@ is full (i.e., is over
-- its limit). /N.B./, a @TBChan@ can be both \"empty\" and \"full\"
-- at the same time, if the initial limit was non-positive. /N.B./,
-- a @TBChan@ may still be full after reading, if 'unGetTBChan' was
-- used to go over the initial limit.
--
-- This is equivalent to: @liftM (<= 0) estimateFreeSlotsTBMChan@
isFullTBChan :: TBChan a -> STM Bool
isFullTBChan (TBChan slots reads _chan) = do
    n <- readTVar slots
    if n <= 0
        then do
            m <- readTVar reads
            let n' = n + m
            writeTVar slots $! n'
            writeTVar reads 0
            return $! n' <= 0
        else return False
{-
-- The above saves an extraneous comparison of n\/n' against 0
-- compared to the more obvious:
isFullTBChan self = do
    n <- estimateFreeSlotsTBChan self
    return $! n <= 0
-}


-- | Estimate the number of free slots. If the result is positive,
-- then it's a minimum bound; if it's non-positive then it's exact.
-- It will only be negative if the initial limit was negative or
-- if 'unGetTBChan' was used to go over the initial limit.
--
-- This function always contends with writers, but only contends
-- with readers when it has to; compare against 'freeSlotsTBChan'.
estimateFreeSlotsTBChan :: TBChan a -> STM Int
estimateFreeSlotsTBChan (TBChan slots reads _chan) = do
    n <- readTVar slots
    if n > 0
        then return n
        else do
            m <- readTVar reads
            let n' = n + m
            writeTVar slots $! n'
            writeTVar reads 0
            return n'


-- | Return the exact number of free slots. The result can be
-- negative if the initial limit was negative or if 'unGetTBChan'
-- was used to go over the initial limit.
--
-- This function always contends with both readers and writers;
-- compare against 'estimateFreeSlotsTBChan'.
freeSlotsTBChan :: TBChan a -> STM Int
freeSlotsTBChan (TBChan slots reads _chan) = do
    n <- readTVar slots
    m <- readTVar reads
    let n' = n + m
    writeTVar slots $! n'
    writeTVar reads 0
    return n'

----------------------------------------------------------------
----------------------------------------------------------- fin.
