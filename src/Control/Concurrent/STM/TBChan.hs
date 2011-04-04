{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}
----------------------------------------------------------------
--                                                    2011.04.03
-- |
-- Module      :  Control.Concurrent.STM.TBChan
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC STM, DeriveDataTypeable)
--
-- A version of "Control.Concurrent.STM.TChan" where the queue is
-- bounded in length.
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

-- | 'TBChan' is an abstract type representing a bounded FIFO
-- channel.
data TBChan a = TBChan !(TVar Int) !(TChan a)
    deriving (Typeable)


-- | Build and returns a new instance of 'TBChan' with the given
-- capacity. /N.B./, we do not verify the capacity is positive, but
-- if it is non-positive then 'writeTBChan' will always retry and
-- 'isFullTBChan' will always be true.
newTBChan :: Int -> STM (TBChan a)
newTBChan n = do
    limit <- newTVar n
    chan  <- newTChan
    return (TBChan limit chan)


-- | @IO@ version of 'newTBChan'. This is useful for creating
-- top-level 'TBChan's using 'unsafePerformIO', because using
-- 'atomically' inside 'unsafePerformIO' isn't possible.
newTBChanIO :: Int -> IO (TBChan a)
newTBChanIO n = do
    limit <- newTVarIO n
    chan  <- newTChanIO
    return (TBChan limit chan)


-- | Read the next value from the 'TBChan', retrying if the channel
-- is empty.
readTBChan :: TBChan a -> STM a
readTBChan (TBChan limit chan) = do
    x <- readTChan chan
    modifyTVar' limit (1 +)
    return x


-- | A version of 'readTBChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTBChan :: TBChan a -> STM (Maybe a)
tryReadTBChan (TBChan limit chan) = do
    mx <- tryReadTChan chan
    case mx of
        Nothing -> return Nothing
        Just _x -> do
            modifyTVar' limit (1 +)
            return mx


-- | Get the next value from the 'TBChan' without removing it,
-- retrying if the channel is empty.
peekTBChan :: TBChan a -> STM a
peekTBChan (TBChan _limit chan) =
    peekTChan chan


-- | A version of 'peekTBChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTBChan :: TBChan a -> STM (Maybe a)
tryPeekTBChan (TBChan _limit chan) =
    tryPeekTChan chan


-- | Write a value to a 'TBChan', retrying if the channel is full.
writeTBChan :: TBChan a -> a -> STM ()
writeTBChan self@(TBChan limit chan) x = do
    b <- isFullTBChan self
    if b
        then retry
        else do
            writeTChan chan x
            modifyTVar' limit (subtract 1)


-- | Put a data item back onto a channel, where it will be the next
-- item read. /N.B./, this could allow the channel to temporarily
-- become longer than the specified limit, which is necessary to
-- ensure that the item is indeed the next one read.
unGetTBChan :: TBChan a -> a -> STM ()
unGetTBChan (TBChan limit chan) x = do
    unGetTChan chan x
    modifyTVar' limit (subtract 1)


-- | Returns @True@ if the supplied 'TBChan' is empty (i.e., has
-- no elements). /N.B./, a 'TBChan' can be both ``empty'' and
-- ``full'' at the same time, if the initial limit was non-positive.
isEmptyTBChan :: TBChan a -> STM Bool
isEmptyTBChan (TBChan _limit chan) =
    isEmptyTChan chan


-- | Returns @True@ if the supplied 'TBChan' is full (i.e., is over
-- its limit). /N.B./, a 'TBChan' can be both ``empty'' and ``full''
-- at the same time, if the initial limit was non-positive.
isFullTBChan :: TBChan a -> STM Bool
isFullTBChan (TBChan limit _chan) = do
    n <- readTVar limit
    return $! n <= 0

----------------------------------------------------------------
----------------------------------------------------------- fin.
