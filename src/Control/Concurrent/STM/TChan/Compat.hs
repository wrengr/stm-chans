{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}
----------------------------------------------------------------
--                                                    2011.04.03
-- |
-- Module      :  Control.Concurrent.STM.TChan.Compat
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC STM, CPP)
--
-- Compatibility layer for older versions of the @stm@ library.
-- Actually, this isn't needed for the @stm-chans@ library. Rather
-- it's provided as a convenience for others. We define 'tryReadTChan',
-- 'peekTChan', and 'tryPeekTChan' which @stm-X.X.X@ lacks. These
-- implementations are less efficient than the package versions due
-- to the 'TChan' type being abstract. However, this module uses
-- Cabal-style CPP macros in order to use the package versions when
-- available.
----------------------------------------------------------------
module Control.Concurrent.STM.TChan.Compat
    (
    -- * The TChan type
      TChan
    -- ** Creating TChans
    , newTChan     -- :: STM (TChan a)
    , newTChanIO   -- :: IO  (TChan a)
    , dupTChan     -- :: TChan a -> STM (TChan a)
    -- ** Reading from TChans
    , readTChan    -- :: TChan a -> STM a
    , tryReadTChan -- :: TChan a -> STM (Maybe a)
    , peekTChan    -- :: TChan a -> STM a
    , tryPeekTChan -- :: TChan a -> STM (Maybe a)
    , isEmptyTChan -- :: TChan a -> STM Bool
    -- ** Writing to TChans
    , unGetTChan   -- :: TChan a -> a -> STM ()
    , writeTChan   -- :: TChan a -> a -> STM ()
    ) where

import Control.Concurrent.STM.TChan -- N.B., GHC only

-- BUG: What version will these really be added?
#if ! (MIN_VERSION_stm(9,0,0))
import Control.Applicative ((<$>))
import Control.Monad.STM   (STM)

----------------------------------------------------------------

-- | A version of 'readTChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan chan = do
    b <- isEmptyTChan chan
    if b then return Nothing else Just <$> readTChan chan
{- -- Optimized implementation for when patching @stm@
tryReadTChan (TChan read _write) = do
    hd <- readTVar =<< readTVar read
    case hd of
        TNil       -> return Nothing
        TCons a tl -> do
            writeTVar read tl
            return (Just a)
-}


-- | Get the next value from the 'TChan' without removing it,
-- retrying if the channel is empty.
peekTChan :: TChan a -> STM a
peekTChan chan = do
    x <- readTChan chan
    unGetTChan chan x
    return x
{- -- Optimized implementation for when patching @stm@
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
{- -- Optimized implementation for when patching @stm@
tryPeekTChan (TChan read _write) = do
    hd <- readTVar =<< readTVar read
    case hd of
        TNil      -> return Nothing
        TCons a _ -> return (Just a)
-}

#endif

----------------------------------------------------------------
----------------------------------------------------------- fin.
