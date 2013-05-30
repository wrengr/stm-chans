{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
----------------------------------------------------------------
--                                                    2013.05.29
-- |
-- Module      :  Control.Concurrent.STM.TBMQueue
-- Copyright   :  Copyright (c) 2011--2013 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC STM, DeriveDataTypeable)
--
-- A version of "Control.Concurrent.STM.TQueue" where the queue is
-- bounded in length and closeable. This combines the abilities of
-- "Control.Concurrent.STM.TBQueue" and "Control.Concurrent.STM.TMQueue".
--
-- /Since: 2.0.0/
----------------------------------------------------------------
module Control.Concurrent.STM.TBMQueue
    (
    -- * The TBMQueue type
      TBMQueue()
    -- ** Creating TBMQueues
    , newTBMQueue
    , newTBMQueueIO
    -- ** Reading from TBMQueues
    , readTBMQueue
    , tryReadTBMQueue
    , peekTBMQueue
    , tryPeekTBMQueue
    -- ** Writing to TBMQueues
    , writeTBMQueue
    , tryWriteTBMQueue
    , unGetTBMQueue
    -- ** Closing TBMQueues
    , closeTBMQueue
    -- ** Predicates
    , isClosedTBMQueue
    , isEmptyTBMQueue
    , isFullTBMQueue
    -- ** Other functionality
    , estimateFreeSlotsTBMQueue
    , freeSlotsTBMQueue
    ) where

import Prelude             hiding (reads)
import Data.Typeable       (Typeable)
import Control.Applicative ((<$>))
import Control.Monad.STM   (STM, retry)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue -- N.B., GHC only

-- N.B., we need a Custom cabal build-type for this to work.
#ifdef __HADDOCK__
import Control.Monad.STM   (atomically)
import System.IO.Unsafe    (unsafePerformIO)
#endif
----------------------------------------------------------------

-- | @TBMQueue@ is an abstract type representing a bounded closeable
-- FIFO queue.
data TBMQueue a = TBMQueue
    {-# UNPACK #-} !(TVar Bool)
    {-# UNPACK #-} !(TVar Int)
    {-# UNPACK #-} !(TVar Int)
    {-# UNPACK #-} !(TQueue a)
    deriving (Typeable)
-- The components are:
-- * Whether the queue has been closed.
-- * How many free slots we /know/ we have available.
-- * How many slots have been freed up by successful reads since
--   the last time the slot count was synchronized by 'isFullTBQueue'.
-- * The underlying TQueue.


-- | Build and returns a new instance of @TBMQueue@ with the given
-- capacity. /N.B./, we do not verify the capacity is positive, but
-- if it is non-positive then 'writeTBMQueue' will always retry and
-- 'isFullTBMQueue' will always be true.
newTBMQueue :: Int -> STM (TBMQueue a)
newTBMQueue n = do
    closed <- newTVar False
    slots  <- newTVar n
    reads  <- newTVar 0
    queue  <- newTQueue
    return (TBMQueue closed slots reads queue)


-- | @IO@ version of 'newTBMQueue'. This is useful for creating
-- top-level @TBMQueue@s using 'unsafePerformIO', because using
-- 'atomically' inside 'unsafePerformIO' isn't possible.
newTBMQueueIO :: Int -> IO (TBMQueue a)
newTBMQueueIO n = do
    closed <- newTVarIO False
    slots  <- newTVarIO n
    reads  <- newTVarIO 0
    queue  <- newTQueueIO
    return (TBMQueue closed slots reads queue)


-- | Read the next value from the @TBMQueue@, retrying if the queue
-- is empty (and not closed). We return @Nothing@ immediately if
-- the queue is closed and empty.
readTBMQueue :: TBMQueue a -> STM (Maybe a)
readTBMQueue (TBMQueue closed _slots reads queue) = do
    b <- readTVar closed
    if b
        then do
            mx <- tryReadTQueue queue
            case mx of
                Nothing -> return mx
                Just _x -> do
                    modifyTVar' reads (1 +)
                    return mx
        else do
            x <- readTQueue queue
            modifyTVar' reads (1 +)
            return (Just x)
{-
-- The above is slightly optimized over the clearer:
readTBMQueue (TBMQueue closed _slots reads queue) =
    b  <- readTVar closed
    b' <- isEmptyTQueue queue
    if b && b'
        then return Nothing
        else do
            x <- readTQueue queue
            modifyTVar' reads (1 +)
            return (Just x)
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | A version of 'readTBMQueue' which does not retry. Instead it
-- returns @Just Nothing@ if the queue is open but no value is
-- available; it still returns @Nothing@ if the queue is closed
-- and empty.
tryReadTBMQueue :: TBMQueue a -> STM (Maybe (Maybe a))
tryReadTBMQueue (TBMQueue closed _slots reads queue) = do
    b <- readTVar closed
    if b
        then do
            mx <- tryReadTQueue queue
            case mx of
                Nothing -> return Nothing
                Just _x -> do
                    modifyTVar' reads (1 +)
                    return (Just mx)
        else do
            mx <- tryReadTQueue queue
            case mx of
                Nothing -> return (Just mx)
                Just _x -> do
                    modifyTVar' reads (1 +)
                    return (Just mx)
{-
-- The above is slightly optimized over the clearer:
tryReadTBMQueue (TBMQueue closed _slots reads queue) =
    b  <- readTVar closed
    b' <- isEmptyTQueue queue
    if b && b'
        then return Nothing
        else do
            mx <- tryReadTBMQueue queue
            case mx of
                Nothing -> return (Just mx)
                Just _x -> do
                    modifyTVar' reads (1 +)
                    return (Just mx)
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | Get the next value from the @TBMQueue@ without removing it,
-- retrying if the queue is empty.
peekTBMQueue :: TBMQueue a -> STM (Maybe a)
peekTBMQueue (TBMQueue closed _slots _reads queue) = do
    b <- readTVar closed
    if b
        then do
            b' <- isEmptyTQueue queue
            if b'
                then return Nothing
                else Just <$> peekTQueue queue
        else Just <$> peekTQueue queue
{-
-- The above is lazier reading from @queue@ than the clearer:
peekTBMQueue (TBMQueue closed _slots _reads queue) = do
    b  <- isEmptyTQueue queue
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else Just <$> peekTQueue queue
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | A version of 'peekTBMQueue' which does not retry. Instead it
-- returns @Just Nothing@ if the queue is open but no value is
-- available; it still returns @Nothing@ if the queue is closed
-- and empty.
tryPeekTBMQueue :: TBMQueue a -> STM (Maybe (Maybe a))
tryPeekTBMQueue (TBMQueue closed _slots _reads queue) = do
    b <- readTVar closed
    if b
        then fmap Just <$> tryPeekTQueue queue
        else Just <$> tryPeekTQueue queue
{-
-- The above is lazier reading from @queue@ (and removes an extraneous isEmptyTQueue when using the compatibility layer) than the clearer:
tryPeekTBMQueue (TBMQueue closed _slots _reads queue) = do
    b  <- isEmptyTQueue queue
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else Just <$> tryPeekTQueue queue
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | Write a value to a @TBMQueue@, retrying if the queue is full.
-- If the queue is closed then the value is silently discarded.
-- Use 'isClosedTBMQueue' to determine if the queue is closed
-- before writing, as needed.
writeTBMQueue :: TBMQueue a -> a -> STM ()
writeTBMQueue self@(TBMQueue closed slots _reads queue) x = do
    b <- readTVar closed
    if b
        then return () -- Discard silently
        else do
            n <- estimateFreeSlotsTBMQueue self
            if n <= 0
                then retry
                else do
                    writeTVar slots $! n - 1
                    writeTQueue queue x


-- | A version of 'writeTBMQueue' which does not retry. Returns @Just
-- True@ if the value was successfully written, @Just False@ if it
-- could not be written (but the queue was open), and @Nothing@
-- if it was discarded (i.e., the queue was closed).
tryWriteTBMQueue :: TBMQueue a -> a -> STM (Maybe Bool)
tryWriteTBMQueue self@(TBMQueue closed slots _reads queue) x = do
    b <- readTVar closed
    if b
        then return Nothing
        else do
            n <- estimateFreeSlotsTBMQueue self
            if n <= 0
                then return (Just False)
                else do
                    writeTVar slots $! n - 1
                    writeTQueue queue x
                    return (Just True)


-- | Put a data item back onto a queue, where it will be the next
-- item read. If the queue is closed then the value is silently
-- discarded; you can use 'peekTBMQueue' to circumvent this in certain
-- circumstances. /N.B./, this could allow the queue to temporarily
-- become longer than the specified limit, which is necessary to
-- ensure that the item is indeed the next one read.
unGetTBMQueue :: TBMQueue a -> a -> STM ()
unGetTBMQueue (TBMQueue closed slots _reads queue) x = do
    b <- readTVar closed
    if b
        then return () -- Discard silently
        else do
            modifyTVar' slots (subtract 1)
            unGetTQueue queue x


-- | Closes the @TBMQueue@, preventing any further writes.
closeTBMQueue :: TBMQueue a -> STM ()
closeTBMQueue (TBMQueue closed _slots _reads _queue) =
    writeTVar closed True


-- | Returns @True@ if the supplied @TBMQueue@ has been closed.
isClosedTBMQueue :: TBMQueue a -> STM Bool
isClosedTBMQueue (TBMQueue closed _slots _reads _queue) =
    readTVar closed

{-
-- | Returns @True@ if the supplied @TBMQueue@ has been closed.
isClosedTBMQueueIO :: TBMQueue a -> IO Bool
isClosedTBMQueueIO (TBMQueue closed _slots _reads _queue) =
    readTVarIO closed
-}


-- | Returns @True@ if the supplied @TBMQueue@ is empty (i.e., has
-- no elements). /N.B./, a @TBMQueue@ can be both ``empty'' and
-- ``full'' at the same time, if the initial limit was non-positive.
isEmptyTBMQueue :: TBMQueue a -> STM Bool
isEmptyTBMQueue (TBMQueue _closed _slots _reads queue) =
    isEmptyTQueue queue


-- | Returns @True@ if the supplied @TBMQueue@ is full (i.e., is
-- over its limit). /N.B./, a @TBMQueue@ can be both ``empty'' and
-- ``full'' at the same time, if the initial limit was non-positive.
-- /N.B./, a @TBMQueue@ may still be full after reading, if
-- 'unGetTBMQueue' was used to go over the initial limit.
--
-- This is equivalent to: @liftM (<= 0) estimateFreeSlotsTBMQueue@
isFullTBMQueue :: TBMQueue a -> STM Bool
isFullTBMQueue (TBMQueue _closed slots reads _queue) = do
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
-- if 'unGetTBMQueue' was used to go over the initial limit.
--
-- This function always contends with writers, but only contends
-- with readers when it has to; compare against 'freeSlotsTBMQueue'.
estimateFreeSlotsTBMQueue :: TBMQueue a -> STM Int
estimateFreeSlotsTBMQueue (TBMQueue _closed slots reads _queue) = do
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
-- negative if the initial limit was negative or if 'unGetTBMQueue'
-- was used to go over the initial limit.
--
-- This function always contends with both readers and writers;
-- compare against 'estimateFreeSlotsTBMQueue'.
freeSlotsTBMQueue :: TBMQueue a -> STM Int
freeSlotsTBMQueue (TBMQueue _closed slots reads _queue) = do
    n <- readTVar slots
    m <- readTVar reads
    let n' = n + m
    writeTVar slots $! n'
    writeTVar reads 0
    return n'

----------------------------------------------------------------
----------------------------------------------------------- fin.
