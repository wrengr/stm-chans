{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
----------------------------------------------------------------
--                                                    2013.05.12
-- |
-- Module      :  Control.Concurrent.STM.TChan.Compat
-- Copyright   :  Copyright (c) 2011--2013 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC STM, CPP)
--
-- Compatibility layer for older versions of the @stm@ library.
-- Namely, we define 'tryReadTChan', 'peekTChan', and 'tryPeekTChan'
-- which @stm < 2.3.0@ lacks; and we define 'newBroadcastTChan' and
-- 'newBroadcastTChanIO' which @stm < 2.4@ lacks. These implementations
-- are less efficient than the package versions due to the 'TChan'
-- type being abstract. However, this module uses Cabal-style CPP
-- macros in order to use the package versions when available.
----------------------------------------------------------------
module Control.Concurrent.STM.TChan.Compat
    (
    -- * The TChan type
      TChan()
    -- ** Creating TChans
    , newTChan              -- :: STM (TChan a)
    , newTChanIO            -- :: IO  (TChan a)
    , dupTChan              -- :: TChan a -> STM (TChan a)
    , newBroadcastTChan     -- :: STM (TChan a)
    , newBroadcastTChanIO   -- :: IO  (TChan a)
    -- ** Reading from TChans
    , readTChan             -- :: TChan a -> STM a
    , tryReadTChan          -- :: TChan a -> STM (Maybe a)
    , peekTChan             -- :: TChan a -> STM a
    , tryPeekTChan          -- :: TChan a -> STM (Maybe a)
    -- ** Writing to TChans
    , unGetTChan            -- :: TChan a -> a -> STM ()
    , writeTChan            -- :: TChan a -> a -> STM ()
    -- ** Predicates
    , isEmptyTChan          -- :: TChan a -> STM Bool
    ) where

import Control.Concurrent.STM.TChan -- N.B., GHC only

#if ! (MIN_VERSION_stm(2,3,0))
import Control.Applicative ((<$>))
import Control.Monad.STM   (STM)
#endif
#if ! (MIN_VERSION_stm(2,4,0))
import Control.Monad.STM   (STM)
import Control.Concurrent.STM.TVar
#endif

----------------------------------------------------------------
#if ! (MIN_VERSION_stm(2,3,0))
-- | A version of 'readTChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan chan = do
    b <- isEmptyTChan chan
    if b then return Nothing else Just <$> readTChan chan
{- -- The optimized implementation in stm-2.3.0
tryReadTChan (TChan read _write) = do
    hd <- readTVar =<< readTVar read
    case hd of
        TNil       -> return Nothing
        TCons a tl -> do
            writeTVar read tl
            return (Just a)
-}


-- | Get the next value from the @TChan@ without removing it,
-- retrying if the channel is empty.
peekTChan :: TChan a -> STM a
peekTChan chan = do
    x <- readTChan chan
    unGetTChan chan x
    return x
{- -- The optimized implementation in stm-2.3.0
peekTChan (TChan read _write) = do
    hd <- readTVar =<< readTVar read
    case hd of
        TNil      -> retry
        TCons a _ -> return a
-}


-- | A version of 'peekTChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTChan :: TChan a -> STM (Maybe a)
tryPeekTChan chan = do
    b <- isEmptyTChan chan
    if b then return Nothing else Just <$> peekTChan chan
{- -- The optimized implementation in stm-2.3.0
tryPeekTChan (TChan read _write) = do
    hd <- readTVar =<< readTVar read
    case hd of
        TNil      -> return Nothing
        TCons a _ -> return (Just a)
-}

#endif

----------------------------------------------------------------
#if ! (MIN_VERSION_stm(2,4,0))
-- BUG: how can we replicate this??

-- | Create a write-only 'TChan'. More precisely, 'readTChan' will
-- 'retry' even after items have been written to the channel. The
-- only way to read a broadcast channel is to duplicate it with
-- 'dupTChan'.
--
-- Consider a server that broadcasts messages to clients:
--
-- >serve :: TChan Message -> Client -> IO loop
-- >serve broadcastChan client = do
-- >    myChan <- dupTChan broadcastChan
-- >    forever $ do
-- >        message <- readTChan myChan
-- >        send client message
--
-- The problem with using 'newTChan' to create the broadcast channel
-- is that if it is only written to and never read, items will pile
-- up in memory. By using 'newBroadcastTChan' to create the broadcast
-- channel, items can be garbage collected after clients have seen
-- them.
newBroadcastTChan :: STM (TChan a)
newBroadcastTChan = do
    -- a la stm-2.4.2 (not stm-2.4 !)
    write_hole <- newTVar TNil
    read <- newTVar (error "reading from a TChan created by newBroadcastTChan; use dupTChan first")
    write <- newTVar write_hole
    return (TChan read write)


-- | @IO@ version of 'newBroadcastTChan'.
newBroadcastTChanIO :: IO (TChan a)
newBroadcastTChanIO = do
    -- a la stm-2.4.2 (which is the same as stm-2.4 !!)
    dummy_hole <- newTVarIO TNil
    write_hole <- newTVarIO TNil
    read  <- newTVarIO dummy_hole
    write <- newTVarIO write_hole
    return (TChan read write)
#endif

----------------------------------------------------------------
----------------------------------------------------------- fin.
