{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
----------------------------------------------------------------
--                                                    2012.05.29
-- |
-- Module      :  Control.Concurrent.STM.TMVar.Compat
-- Copyright   :  Copyright (c) 2011--2013 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (STM, CPP)
--
-- Compatibility layer for older versions of the @stm@ library.
-- Namely, we define 'tryReadTMVar' which @stm < 2.3.0@ lacks. This
-- module uses Cabal-style CPP macros in order to use the package
-- versions when available. This isn't actually used by the @stm-chans@
-- package, but we provide it anyways since we provide compatibility
-- layers for @TVar@ and @TChan@.
--
-- /Since: 1.3.0; Deprecated: 2.1.0 (will be removed in 3.0)/
----------------------------------------------------------------
module Control.Concurrent.STM.TMVar.Compat
    {-# DEPRECATED "stm-chans >= 2.1 requires stm >= 2.4; so this module no longer does anything useful." #-}
    (
    -- * The TMVar type
      TMVar()
    -- ** Creating TMVars
    , newTMVar          -- :: a -> STM (TMVar a)
    , newTMVarIO        -- :: a -> IO  (TMVar a)
    , newEmptyTMVar     -- :: STM (TMVar a)
    , newEmptyTMVarIO   -- :: IO  (TMVar a)
    -- ** Reading from TMVars
    , readTMVar         -- :: TMVar a -> STM a
    , tryReadTMVar      -- :: TMVar a -> STM (Maybe a)
    , takeTMVar         -- :: TMVar a -> STM a
    , tryTakeTMVar      -- :: TMVar a -> STM (Maybe a)
    -- ** Writing to TMVars
    , putTMVar          -- :: TMVar a -> a -> STM ()
    , tryPutTMVar       -- :: TMVar a -> a -> STM Bool
    , swapTMVar         -- :: TMVar a -> a -> STM a
    -- TODO: make another patch for trySwapTMVar?
    -- ** Other capabilities
    , isEmptyTMVar      -- :: TMVar a -> STM Bool
    ) where

import Control.Concurrent.STM.TMVar

#if ! (MIN_VERSION_stm(2,3,0))
import Control.Concurrent.STM (STM)
----------------------------------------------------------------

-- | A version of 'readTMVar' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTMVar :: TMVar a -> STM (Maybe a)
tryReadTMVar var = do
    m <- tryTakeTMVar var
    case m of
        Nothing -> return Nothing
        Just x  -> putTMVar var x >> return (Just x)
{- -- The optimized implementation in stm-2.3.0
tryReadTMVar (TMVar t) = readTVar t
{-# INLINE tryReadTMVar #-}
-}

#endif

----------------------------------------------------------------
----------------------------------------------------------- fin.
