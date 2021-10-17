{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  Control.Concurrent.STM.TBMChan
-- Copyright   :  Copyright (c) 2011--2021 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC STM, DeriveDataTypeable)
--
-- A version of "Control.Concurrent.STM.TChan" where the queue is
-- bounded in length and closeable. This combines the abilities of
-- "Control.Concurrent.STM.TBChan" and "Control.Concurrent.STM.TMChan".
-- This variant incorporates ideas from Thomas M. DuBuisson's
-- @bounded-tchan@ package in order to reduce contention between
-- readers and writers.
----------------------------------------------------------------
module Control.Concurrent.STM.TBMChan
    (
    -- * The TBMChan type
      TBMChan()
    -- ** Creating TBMChans
    , newTBMChan
    , newTBMChanIO
    -- I don't know how to define dupTBMChan with the correct semantics
    -- ** Reading from TBMChans
    , readTBMChan
    , tryReadTBMChan
    , peekTBMChan
    , tryPeekTBMChan
    -- ** Writing to TBMChans
    , writeTBMChan
    , tryWriteTBMChan
    , unGetTBMChan
    -- ** Closing TBMChans
    , closeTBMChan
    -- ** Predicates
    , isClosedTBMChan
    , isEmptyTBMChan
    , isFullTBMChan
    -- ** Other functionality
    , estimateFreeSlotsTBMChan
    , freeSlotsTBMChan
    ) where

import Prelude             hiding (reads)
import Data.Typeable       (Typeable)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import Control.Monad.STM   (STM, retry)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan -- N.B., GHC only
----------------------------------------------------------------

-- | @TBMChan@ is an abstract type representing a bounded closeable
-- FIFO channel.
data TBMChan a = TBMChan
    {-# UNPACK #-} !(TVar Bool)
    {-# UNPACK #-} !(TVar Int)
    {-# UNPACK #-} !(TVar Int)
    {-# UNPACK #-} !(TChan a)
    deriving (Typeable)
-- The components are:
-- * Whether the channel has been closed.
-- * How many free slots we /know/ we have available.
-- * How many slots have been freed up by successful reads since
--   the last time the slot count was synchronized by 'isFullTBChan'.
-- * The underlying TChan.


-- | Build and returns a new instance of @TBMChan@ with the given
-- capacity. /N.B./, we do not verify the capacity is positive, but
-- if it is non-positive then 'writeTBMChan' will always retry and
-- 'isFullTBMChan' will always be true.
newTBMChan :: Int -> STM (TBMChan a)
newTBMChan n = do
    closed <- newTVar False
    slots  <- newTVar n
    reads  <- newTVar 0
    chan   <- newTChan
    return (TBMChan closed slots reads chan)


-- | @IO@ version of 'newTBMChan'. This is useful for creating
-- top-level @TBMChan@s using 'System.IO.Unsafe.unsafePerformIO',
-- because using 'Control.Monad.STM.atomically' inside
-- 'System.IO.Unsafe.unsafePerformIO' isn't possible.
newTBMChanIO :: Int -> IO (TBMChan a)
newTBMChanIO n = do
    closed <- newTVarIO False
    slots  <- newTVarIO n
    reads  <- newTVarIO 0
    chan   <- newTChanIO
    return (TBMChan closed slots reads chan)


-- | Read the next value from the @TBMChan@, retrying if the channel
-- is empty (and not closed). We return @Nothing@ immediately if
-- the channel is closed and empty.
readTBMChan :: TBMChan a -> STM (Maybe a)
readTBMChan (TBMChan closed _slots reads chan) = do
    b <- readTVar closed
    if b
        then do
            mx <- tryReadTChan chan
            case mx of
                Nothing -> return mx
                Just _x -> do
                    modifyTVar' reads (1 +)
                    return mx
        else do
            x <- readTChan chan
            modifyTVar' reads (1 +)
            return (Just x)
{-
-- The above is slightly optimized over the clearer:
readTBMChan (TBMChan closed _slots reads chan) =
    b  <- readTVar closed
    b' <- isEmptyTChan chan
    if b && b'
        then return Nothing
        else do
            x <- readTChan chan
            modifyTVar' reads (1 +)
            return (Just x)
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | A version of 'readTBMChan' which does not retry. Instead it
-- returns @Just Nothing@ if the channel is open but no value is
-- available; it still returns @Nothing@ if the channel is closed
-- and empty.
tryReadTBMChan :: TBMChan a -> STM (Maybe (Maybe a))
tryReadTBMChan (TBMChan closed _slots reads chan) = do
    b <- readTVar closed
    if b
        then do
            mx <- tryReadTChan chan
            case mx of
                Nothing -> return Nothing
                Just _x -> do
                    modifyTVar' reads (1 +)
                    return (Just mx)
        else do
            mx <- tryReadTChan chan
            case mx of
                Nothing -> return (Just mx)
                Just _x -> do
                    modifyTVar' reads (1 +)
                    return (Just mx)
{-
-- The above is slightly optimized over the clearer:
tryReadTBMChan (TBMChan closed _slots reads chan) =
    b  <- readTVar closed
    b' <- isEmptyTChan chan
    if b && b'
        then return Nothing
        else do
            mx <- tryReadTBMChan chan
            case mx of
                Nothing -> return (Just mx)
                Just _x -> do
                    modifyTVar' reads (1 +)
                    return (Just mx)
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | Get the next value from the @TBMChan@ without removing it,
-- retrying if the channel is empty.
peekTBMChan :: TBMChan a -> STM (Maybe a)
peekTBMChan (TBMChan closed _slots _reads chan) = do
    b <- readTVar closed
    if b
        then do
            b' <- isEmptyTChan chan
            if b'
                then return Nothing
                else Just <$> peekTChan chan
        else Just <$> peekTChan chan
{-
-- The above is lazier reading from @chan@ than the clearer:
peekTBMChan (TBMChan closed _slots _reads chan) = do
    b  <- isEmptyTChan chan
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else Just <$> peekTChan chan
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | A version of 'peekTBMChan' which does not retry. Instead it
-- returns @Just Nothing@ if the channel is open but no value is
-- available; it still returns @Nothing@ if the channel is closed
-- and empty.
tryPeekTBMChan :: TBMChan a -> STM (Maybe (Maybe a))
tryPeekTBMChan (TBMChan closed _slots _reads chan) = do
    b <- readTVar closed
    if b
        then fmap Just <$> tryPeekTChan chan
        else Just <$> tryPeekTChan chan
{-
-- The above is lazier reading from @chan@ (and removes an extraneous isEmptyTChan when using the compatibility layer) than the clearer:
tryPeekTBMChan (TBMChan closed _slots _reads chan) = do
    b  <- isEmptyTChan chan
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else Just <$> tryPeekTChan chan
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | Write a value to a @TBMChan@, retrying if the channel is full.
-- If the channel is closed then the value is silently discarded.
-- Use 'isClosedTBMChan' to determine if the channel is closed
-- before writing, as needed.
writeTBMChan :: TBMChan a -> a -> STM ()
writeTBMChan self@(TBMChan closed slots _reads chan) x = do
    b <- readTVar closed
    if b
        then return () -- Discard silently
        else do
            n <- estimateFreeSlotsTBMChan self
            if n <= 0
                then retry
                else do
                    writeTVar slots $! n - 1
                    writeTChan chan x


-- | A version of 'writeTBMChan' which does not retry. Returns @Just
-- True@ if the value was successfully written, @Just False@ if it
-- could not be written (but the channel was open), and @Nothing@
-- if it was discarded (i.e., the channel was closed).
tryWriteTBMChan :: TBMChan a -> a -> STM (Maybe Bool)
tryWriteTBMChan self@(TBMChan closed slots _reads chan) x = do
    b <- readTVar closed
    if b
        then return Nothing
        else do
            n <- estimateFreeSlotsTBMChan self
            if n <= 0
                then return (Just False)
                else do
                    writeTVar slots $! n - 1
                    writeTChan chan x
                    return (Just True)


-- | Put a data item back onto a channel, where it will be the next
-- item read. If the channel is closed then the value is silently
-- discarded; you can use 'peekTBMChan' to circumvent this in certain
-- circumstances. /N.B./, this could allow the channel to temporarily
-- become longer than the specified limit, which is necessary to
-- ensure that the item is indeed the next one read.
unGetTBMChan :: TBMChan a -> a -> STM ()
unGetTBMChan (TBMChan closed slots _reads chan) x = do
    b <- readTVar closed
    if b
        then return () -- Discard silently
        else do
            modifyTVar' slots (subtract 1)
            unGetTChan chan x


-- | Closes the @TBMChan@, preventing any further writes.
closeTBMChan :: TBMChan a -> STM ()
closeTBMChan (TBMChan closed _slots _reads _chan) =
    writeTVar closed True


-- | Returns @True@ if the supplied @TBMChan@ has been closed.
isClosedTBMChan :: TBMChan a -> STM Bool
isClosedTBMChan (TBMChan closed _slots _reads _chan) =
    readTVar closed

{-
-- | Returns @True@ if the supplied @TBMChan@ has been closed.
isClosedTBMChanIO :: TBMChan a -> IO Bool
isClosedTBMChanIO (TBMChan closed _slots _reads _chan) =
    readTVarIO closed
-}


-- | Returns @True@ if the supplied @TBMChan@ is empty (i.e., has
-- no elements). /N.B./, a @TBMChan@ can be both \"empty\" and
-- \"full\" at the same time, if the initial limit was non-positive.
isEmptyTBMChan :: TBMChan a -> STM Bool
isEmptyTBMChan (TBMChan _closed _slots _reads chan) =
    isEmptyTChan chan


-- | Returns @True@ if the supplied @TBMChan@ is full (i.e., is
-- over its limit). /N.B./, a @TBMChan@ can be both \"empty\" and
-- \"full\" at the same time, if the initial limit was non-positive.
-- /N.B./, a @TBMChan@ may still be full after reading, if
-- 'unGetTBMChan' was used to go over the initial limit.
--
-- This is equivalent to: @liftM (<= 0) estimateFreeSlotsTBMChan@
isFullTBMChan :: TBMChan a -> STM Bool
isFullTBMChan (TBMChan _closed slots reads _chan) = do
    n <- readTVar slots
    if n <= 0
        then do
            m <- readTVar reads
            let n' = n + m
            writeTVar slots $! n'
            writeTVar reads 0
            return $! n' <= 0
        else return False


-- | Estimate the number of free slots. If the result is positive,
-- then it's a minimum bound; if it's non-positive then it's exact.
-- It will only be negative if the initial limit was negative or
-- if 'unGetTBMChan' was used to go over the initial limit.
--
-- This function always contends with writers, but only contends
-- with readers when it has to; compare against 'freeSlotsTBMChan'.
estimateFreeSlotsTBMChan :: TBMChan a -> STM Int
estimateFreeSlotsTBMChan (TBMChan _closed slots reads _chan) = do
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
-- negative if the initial limit was negative or if 'unGetTBMChan'
-- was used to go over the initial limit.
--
-- This function always contends with both readers and writers;
-- compare against 'estimateFreeSlotsTBMChan'.
freeSlotsTBMChan :: TBMChan a -> STM Int
freeSlotsTBMChan (TBMChan _closed slots reads _chan) = do
    n <- readTVar slots
    m <- readTVar reads
    let n' = n + m
    writeTVar slots $! n'
    writeTVar reads 0
    return n'

----------------------------------------------------------------
----------------------------------------------------------- fin.
