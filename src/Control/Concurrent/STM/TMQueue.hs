{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
----------------------------------------------------------------
--                                                    2013.05.29
-- |
-- Module      :  Control.Concurrent.STM.TMQueue
-- Copyright   :  Copyright (c) 2011--2013 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC STM, DeriveDataTypeable)
--
-- A version of "Control.Concurrent.STM.TQueue" where the queue is
-- closeable. This is similar to a @TQueue (Maybe a)@ with a
-- monotonicity guarantee that once there's a @Nothing@ there will
-- always be @Nothing@.
--
-- /Since: 2.0.0/
----------------------------------------------------------------
module Control.Concurrent.STM.TMQueue
    (
    -- * The TMQueue type
      TMQueue()
    -- ** Creating TMQueues
    , newTMQueue
    , newTMQueueIO
    -- ** Reading from TMQueues
    , readTMQueue
    , tryReadTMQueue
    , peekTMQueue
    , tryPeekTMQueue
    -- ** Writing to TMQueues
    , writeTMQueue
    , unGetTMQueue
    -- ** Closing TMQueues
    , closeTMQueue
    -- ** Predicates
    , isClosedTMQueue
    , isEmptyTMQueue
    ) where

import Data.Typeable       (Typeable)
import Control.Applicative ((<$>))
import Control.Monad.STM   (STM)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue -- N.B., GHC only

-- N.B., we need a Custom cabal build-type for this to work.
#ifdef __HADDOCK__
import Control.Monad.STM   (atomically)
import System.IO.Unsafe    (unsafePerformIO)
#endif
----------------------------------------------------------------

-- | @TMQueue@ is an abstract type representing a closeable FIFO
-- queue.
data TMQueue a = TMQueue
    {-# UNPACK #-} !(TVar Bool)
    {-# UNPACK #-} !(TQueue a)
    deriving Typeable


-- | Build and returns a new instance of @TMQueue@.
newTMQueue :: STM (TMQueue a)
newTMQueue = do
    closed <- newTVar False
    queue  <- newTQueue
    return (TMQueue closed queue)


-- | @IO@ version of 'newTMQueue'. This is useful for creating
-- top-level @TMQueue@s using 'unsafePerformIO', because using
-- 'atomically' inside 'unsafePerformIO' isn't possible.
newTMQueueIO :: IO (TMQueue a)
newTMQueueIO = do
    closed <- newTVarIO False
    queue  <- newTQueueIO
    return (TMQueue closed queue)


-- | Read the next value from the @TMQueue@, retrying if the queue
-- is empty (and not closed). We return @Nothing@ immediately if
-- the queue is closed and empty.
readTMQueue :: TMQueue a -> STM (Maybe a)
readTMQueue (TMQueue closed queue) = do
    b <- readTVar closed
    if b
        then tryReadTQueue queue
        else Just <$> readTQueue queue
{-
-- The above is lazier reading from @queue@, and slightly optimized, compared to the clearer:
readTMQueue (TMQueue closed queue) = do
    b  <- isEmptyTQueue queue
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else Just <$> readTQueue queue
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | A version of 'readTMQueue' which does not retry. Instead it
-- returns @Just Nothing@ if the queue is open but no value is
-- available; it still returns @Nothing@ if the queue is closed
-- and empty.
tryReadTMQueue :: TMQueue a -> STM (Maybe (Maybe a))
tryReadTMQueue (TMQueue closed queue) = do
    b <- readTVar closed
    if b
        then fmap Just <$> tryReadTQueue queue
        else Just <$> tryReadTQueue queue
{-
-- The above is lazier reading from @queue@ (and removes an extraneous isEmptyTQueue when using the compatibility layer) than the clearer:
tryReadTMQueue (TMQueue closed queue) = do
    b  <- isEmptyTQueue queue
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else Just <$> tryReadTQueue queue
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | Get the next value from the @TMQueue@ without removing it,
-- retrying if the queue is empty.
peekTMQueue :: TMQueue a -> STM (Maybe a)
peekTMQueue (TMQueue closed queue) = do
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
peekTMQueue (TMQueue closed queue) = do
    b  <- isEmptyTQueue queue
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else Just <$> peekTQueue queue
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | A version of 'peekTMQueue' which does not retry. Instead it
-- returns @Just Nothing@ if the queue is open but no value is
-- available; it still returns @Nothing@ if the queue is closed
-- and empty.
tryPeekTMQueue :: TMQueue a -> STM (Maybe (Maybe a))
tryPeekTMQueue (TMQueue closed queue) = do
    b <- readTVar closed
    if b
        then fmap Just <$> tryPeekTQueue queue
        else Just <$> tryPeekTQueue queue
{-
-- The above is lazier reading from @queue@ (and removes an extraneous isEmptyTQueue when using the compatibility layer) than the clearer:
tryPeekTMQueue (TMQueue closed queue) = do
    b  <- isEmptyTQueue queue
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else Just <$> tryPeekTQueue queue
-- TODO: compare Core and benchmarks; is the loss of clarity worth it?
-}


-- | Write a value to a @TMQueue@. If the queue is closed then the
-- value is silently discarded. Use 'isClosedTMQueue' to determine
-- if the queue is closed before writing, as needed.
writeTMQueue :: TMQueue a -> a -> STM ()
writeTMQueue (TMQueue closed queue) x = do
    b <- readTVar closed
    if b
        then return () -- discard silently
        else writeTQueue queue x


-- | Put a data item back onto a queue, where it will be the next
-- item read. If the queue is closed then the value is silently
-- discarded; you can use 'peekTMQueue' to circumvent this in certain
-- circumstances.
unGetTMQueue :: TMQueue a -> a -> STM ()
unGetTMQueue (TMQueue closed queue) x = do
    b <- readTVar closed
    if b
        then return () -- discard silently
        else unGetTQueue queue x


-- | Closes the @TMQueue@, preventing any further writes.
closeTMQueue :: TMQueue a -> STM ()
closeTMQueue (TMQueue closed _queue) =
    writeTVar closed True


-- | Returns @True@ if the supplied @TMQueue@ has been closed.
isClosedTMQueue :: TMQueue a -> STM Bool
isClosedTMQueue (TMQueue closed _queue) =
    readTVar closed

{-
-- | Returns @True@ if the supplied @TMQueue@ has been closed.
isClosedTMQueueIO :: TMQueue a -> IO Bool
isClosedTMQueueIO (TMQueue closed _queue) =
    readTVarIO closed
-}


-- | Returns @True@ if the supplied @TMQueue@ is empty.
isEmptyTMQueue :: TMQueue a -> STM Bool
isEmptyTMQueue (TMQueue _closed queue) =
    isEmptyTQueue queue

----------------------------------------------------------------
----------------------------------------------------------- fin.
