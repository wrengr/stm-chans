{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}
----------------------------------------------------------------
--                                                    2011.04.17
-- |
-- Module      :  Control.Concurrent.STM.TBMChan1
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC STM, DeriveDataTypeable)
--
-- A version of "Control.Concurrent.STM.TChan" where the queue is
-- bounded in length and closeable. This combines the abilities of
-- "Control.Concurrent.STM.TBChan" and "Control.Concurrent.STM.TMChan".
----------------------------------------------------------------
module Control.Concurrent.STM.TBMChan1
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
    , freeSlotsTBMChan
    ) where

import Data.Typeable       (Typeable)
import Control.Applicative ((<$>))
import Control.Monad.STM   (STM, retry)
import Control.Concurrent.STM.TVar.Compat
import Control.Concurrent.STM.TChan.Compat -- N.B., GHC only

-- N.B., we need a Custom cabal build-type for this to work.
#ifdef __HADDOCK__
import Control.Monad.STM   (atomically)
import System.IO.Unsafe    (unsafePerformIO)
#endif
----------------------------------------------------------------

-- | @TBMChan@ is an abstract type representing a bounded closeable
-- FIFO channel.
data TBMChan a = TBMChan !(TVar Bool) !(TVar Int) !(TChan a)
    deriving Typeable


-- | Build and returns a new instance of @TBMChan@ with the given
-- capacity. /N.B./, we do not verify the capacity is positive, but
-- if it is non-positive then 'writeTBMChan' will always retry and
-- 'isFullTBMChan' will always be true.
newTBMChan :: Int -> STM (TBMChan a)
newTBMChan n = do
    closed <- newTVar False
    limit  <- newTVar n
    chan   <- newTChan
    return (TBMChan closed limit chan)


-- | @IO@ version of 'newTBMChan'. This is useful for creating
-- top-level @TBMChan@s using 'unsafePerformIO', because using
-- 'atomically' inside 'unsafePerformIO' isn't possible.
newTBMChanIO :: Int -> IO (TBMChan a)
newTBMChanIO n = do
    closed <- newTVarIO False
    limit  <- newTVarIO n
    chan   <- newTChanIO
    return (TBMChan closed limit chan)


-- | Read the next value from the @TBMChan@, retrying if the channel
-- is empty (and not closed). We return @Nothing@ immediately if
-- the channel is closed and empty.
readTBMChan :: TBMChan a -> STM (Maybe a)
readTBMChan (TBMChan closed limit chan) = do
    b  <- isEmptyTChan chan
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else do
            x <- readTChan chan
            modifyTVar' limit (1 +)
            return (Just x)


-- | A version of 'readTBMChan' which does not retry. Instead it
-- returns @Just Nothing@ if the channel is open but no value is
-- available; it still returns @Nothing@ if the channel is closed
-- and empty.
tryReadTBMChan :: TBMChan a -> STM (Maybe (Maybe a))
tryReadTBMChan (TBMChan closed limit chan) = do
    b  <- isEmptyTChan chan
    b' <- readTVar closed
    if b && b'
        then return Nothing
        else do
            mx <- tryReadTChan chan
            case mx of
                Nothing -> return (Just Nothing)
                Just _x -> do
                    modifyTVar' limit (1 +)
                    return (Just mx)


-- | Get the next value from the @TBMChan@ without removing it,
-- retrying if the channel is empty.
peekTBMChan :: TBMChan a -> STM (Maybe a)
peekTBMChan (TBMChan closed _limit chan) = do
    b  <- isEmptyTChan chan
    b' <- readTVar closed
    if b && b' 
        then return Nothing
        else Just <$> peekTChan chan


-- | A version of 'peekTBMChan' which does not retry. Instead it
-- returns @Just Nothing@ if the channel is open but no value is
-- available; it still returns @Nothing@ if the channel is closed
-- and empty.
tryPeekTBMChan :: TBMChan a -> STM (Maybe (Maybe a))
tryPeekTBMChan (TBMChan closed _limit chan) = do
    b  <- isEmptyTChan chan
    b' <- readTVar closed
    if b && b' 
        then return Nothing
        else Just <$> tryPeekTChan chan


-- | Write a value to a @TBMChan@, retrying if the channel is full.
-- If the channel is closed then the value is silently discarded.
-- Use 'isClosedTBMChan' to determine if the channel is closed
-- before writing, as needed.
writeTBMChan :: TBMChan a -> a -> STM ()
writeTBMChan self@(TBMChan closed limit chan) x = do
    b <- readTVar closed
    if b
        then return () -- Discard silently
        else do
            b' <- isFullTBMChan self
            if b'
                then retry
                else do
                    writeTChan chan x
                    modifyTVar' limit (subtract 1)


-- | A version of 'writeTBMChan' which does not retry. Returns @Just
-- True@ if the value was successfully written, @Just False@ if it
-- could not be written (but the channel was open), and @Nothing@
-- if it was discarded (i.e., the channel was closed).
tryWriteTBMChan :: TBMChan a -> a -> STM (Maybe Bool)
tryWriteTBMChan self@(TBMChan closed limit chan) x = do
    b <- readTVar closed
    if b
        then return Nothing
        else do
            b' <- isFullTBMChan self
            if b'
                then return (Just False)
                else do
                    writeTChan chan x
                    modifyTVar' limit (subtract 1)
                    return (Just True)


-- | Put a data item back onto a channel, where it will be the next
-- item read. If the channel is closed then the value is silently
-- discarded; you can use 'peekTBMChan' to circumvent this in certain
-- circumstances. /N.B./, this could allow the channel to temporarily
-- become longer than the specified limit, which is necessary to
-- ensure that the item is indeed the next one read.
unGetTBMChan :: TBMChan a -> a -> STM ()
unGetTBMChan (TBMChan closed limit chan) x = do
    b <- readTVar closed
    if b
        then return () -- Discard silently
        else do
            unGetTChan chan x
            modifyTVar' limit (subtract 1)


-- | Closes the @TBMChan@, preventing any further writes.
closeTBMChan :: TBMChan a -> STM ()
closeTBMChan (TBMChan closed _limit _chan) =
    writeTVar closed True


-- | Returns @True@ if the supplied @TBMChan@ has been closed.
isClosedTBMChan :: TBMChan a -> STM Bool
isClosedTBMChan (TBMChan closed _limit _chan) =
    readTVar closed


-- | Returns @True@ if the supplied @TBMChan@ is empty (i.e., has
-- no elements). /N.B./, a @TBMChan@ can be both ``empty'' and
-- ``full'' at the same time, if the initial limit was non-positive.
isEmptyTBMChan :: TBMChan a -> STM Bool
isEmptyTBMChan (TBMChan _closed _limit chan) =
    isEmptyTChan chan


-- | Returns @True@ if the supplied @TBMChan@ is full (i.e., is
-- over its limit). /N.B./, a @TBMChan@ can be both ``empty'' and
-- ``full'' at the same time, if the initial limit was non-positive.
-- /N.B./, a @TBMChan@ may still be full after reading, if
-- 'unGetTBMChan' was used to go over the initial limit.
isFullTBMChan :: TBMChan a -> STM Bool
isFullTBMChan (TBMChan _closed limit _chan) = do
    n <- readTVar limit
    return $! n <= 0


-- | Return the exact number of free slots. The result can be
-- negative if the initial limit was negative or if 'unGetTBMChan'
-- was used to go over the initial limit.
freeSlotsTBMChan :: TBMChan a -> STM Int
freeSlotsTBMChan (TBMChan _closed limit _chan) =
    readTVar limit

----------------------------------------------------------------
----------------------------------------------------------- fin.
