{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}
----------------------------------------------------------------
--                                                    2011.04.05
-- |
-- Module      :  Control.Concurrent.STM.TBChan2
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC STM, DeriveDataTypeable)
--
-- A version of "Control.Concurrent.STM.TChan" where the queue is
-- bounded in length. This variant incorporates ideas from Thomas
-- M. DuBuisson's @bounded-tchan@ package in order to reduce
-- contention between readers and writers.
--
-- TODO: still need to benchmark this to verify Thomas' performance
-- numbers (though they're surely true). Once that's done, this
-- should replace Control.Concurrent.STM.TBChan and add Thomas to
-- the AUTHORS file. Also, reimplement TBMChan to follow suit.
----------------------------------------------------------------
module Control.Concurrent.STM.TBChan2
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
    ) where

import Data.Typeable     (Typeable)
import Control.Monad.STM (STM, retry)
import Control.Concurrent.STM.TVar.Compat
import Control.Concurrent.STM.TChan.Compat -- N.B., GHC only

-- N.B., we need a Custom cabal build-type for this to work.
#ifdef __HADDOCK__
import Control.Monad.STM (atomically)
import System.IO.Unsafe  (unsafePerformIO)
#endif
----------------------------------------------------------------

-- | @TBChan@ is an abstract type representing a bounded FIFO
-- channel.
data TBChan a = TBChan !(TVar Int) !(TVar Int) !(TChan a)
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
-- top-level @TBChan@s using 'unsafePerformIO', because using
-- 'atomically' inside 'unsafePerformIO' isn't possible.
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
    b <- isFullTBChan self
    if b
        then retry
        else do
            modifyTVar' slots (subtract 1)
            writeTChan chan x


-- | A version of 'writeTBChan' which does not retry. Returns @True@
-- if the value was successfully written, and @False@ otherwise.
tryWriteTBChan :: TBChan a -> a -> STM Bool
tryWriteTBChan self@(TBChan slots _reads chan) x = do
    b <- isFullTBChan self
    if b
        then return False
        else do
            modifyTVar' slots (subtract 1)
            writeTChan chan x
            return True


-- | Put a data item back onto a channel, where it will be the next
-- item read. /N.B./, this could allow the channel to temporarily
-- become longer than the specified limit, which is necessary to
-- ensure that the item is indeed the next one read.
unGetTBChan :: TBChan a -> a -> STM ()
unGetTBChan (TBChan slots _reads chan) x = do
    modifyTVar' slots (subtract 1)
    unGetTChan chan x


-- | Returns @True@ if the supplied @TBChan@ is empty (i.e., has
-- no elements). /N.B./, a @TBChan@ can be both ``empty'' and
-- ``full'' at the same time, if the initial limit was non-positive.
isEmptyTBChan :: TBChan a -> STM Bool
isEmptyTBChan (TBChan _slots _reads chan) =
    isEmptyTChan chan


-- | Returns @True@ if the supplied @TBChan@ is full (i.e., is over
-- its limit). /N.B./, a @TBChan@ can be both ``empty'' and ``full''
-- at the same time, if the initial limit was non-positive. /N.B./,
-- a @TBChan@ may still be full after reading, if 'unGetTBChan' was
-- used to go over the initial limit.
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

----------------------------------------------------------------
----------------------------------------------------------- fin.
