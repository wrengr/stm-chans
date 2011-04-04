{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}
----------------------------------------------------------------
--                                                    2011.04.03
-- |
-- Module      :  Control.Concurrent.STM.TVar.Compat
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (STM, CPP)
--
-- Compatibility layer for older versions of the @stm@ library.
-- Namely, we define 'readTVarIO' which @stm-2.1.1@ lacks; and we
-- define 'modifyTVar', 'modifyTVar'', and 'swapTVar' which @stm-X.X.X@
-- lacks. This module uses Cabal-style CPP macros in order to use
-- the package versions when available.
----------------------------------------------------------------
module Control.Concurrent.STM.TVar.Compat
    ( TVar
    , newTVar       -- :: a -> STM (TVar a)
    , newTVarIO     -- :: a -> IO  (TVar a)
    , readTVar      -- :: TVar a -> STM a
    , readTVarIO    -- :: TVar a -> IO  a
    , writeTVar     -- :: TVar a -> a -> STM ()
    , modifyTVar    -- :: TVar a -> (a -> a) -> STM ()
    , modifyTVar'   -- :: TVar a -> (a -> a) -> STM ()
    , swapTVar      -- :: TVar a -> a -> STM a
    , registerDelay -- :: Int -> IO (TVar Bool)
    ) where

import Control.Concurrent.STM.TVar

#if ! (MIN_VERSION_stm(2,1,2))
import Control.Concurrent.STM (atomically)
#endif

-- What version will these really be added?
#if ! (MIN_VERSION_stm(9,0,0))
import Control.Concurrent.STM (STM)
#endif
----------------------------------------------------------------

#if ! (MIN_VERSION_stm(2,1,2))
-- | Return the current value stored in a TVar. This is equivalent to
--
-- > readTVarIO = atomically . readTVar
--
-- but works much faster (on @stm >= 2.1.2@), because it doesn't
-- perform a complete transaction, it just reads the current value
-- of the TVar.
readTVarIO :: TVar a -> IO a
readTVarIO = atomically . readTVar
{-# INLINE readTVarIO #-}
#endif


-- What version will these really be added?
#if ! (MIN_VERSION_stm(9,0,0))

-- Like 'modifyIORef' but for 'TVar'.
-- | Mutate the contents of a 'TVar'. /N.B./, this version is
-- non-strict.
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = do
    x <- readTVar var
    writeTVar var (f x)
{-# INLINE modifyTVar #-}


-- | Strict version of 'modifyTVar'.
modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x
{-# INLINE modifyTVar' #-}


-- Like 'swapTMVar' but for 'TVar'.
-- | Swap the contents of a 'TVar' for a new value.
swapTVar :: TVar a -> a -> STM a
swapTVar var new = do
    old <- readTVar var
    writeTVar var new
    return old
{-# INLINE swapTVar #-}

#endif

----------------------------------------------------------------
----------------------------------------------------------- fin.
