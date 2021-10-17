stm-chans
=========
[![Hackage version](https://img.shields.io/hackage/v/stm-chans.svg?style=flat)](https://hackage.haskell.org/package/stm-chans) 
[![Build Status](https://github.com/wrengr/stm-chans/workflows/ci/badge.svg)](https://github.com/wrengr/stm-chans/actions?query=workflow%3Aci)
[![Dependencies](https://img.shields.io/hackage-deps/v/stm-chans.svg?style=flat)](http://packdeps.haskellers.com/specific?package=stm-chans)

This package offers a collection of channel types, similar to
`Control.Concurrent.STM.{TChan,TQueue}` but with additional features.
In particular we offer the following data types:

* `Control.Concurrent.STM.TBChan`:  Bounded FIFO channels.
    When the channel is full, writers will block/retry. This ensures
    that the writers do not get too far ahead of the readers, which
    helps to  make sure that memory and cpu resources are used
    responsibly.
* `Control.Concurrent.STM.TMChan`:   Closeable FIFO channels.
* `Control.Concurrent.STM.TMQueue`:  Closeable FIFO queues.
    Like `TChan (Maybe a)` but with a monotonicity guarantee that
    once `Nothing` is returned all future reads will be `Nothing`
    as well.
* `Control.Concurrent.STM.TBMChan`:  Bounded Closeable FIFO channels.
* `Control.Concurrent.STM.TBMQueue`: Bounded Closeable FIFO queues.
    Combines the capabilities of `TBChan` and `TMChan`.


## Install

In general, this is a simple package and should be easy to install.
It does require GHC however, because it relies on the
Control.Concurrent.STM.TChan type which (for some unknown reason)
is GHC-only. With the cabal-install program you can just do:

    $> cabal install stm-chans

Or if you don't have cabal-install, then you can use the Cabal
library:

    $> runhaskell Setup.hs configure
    $> runhaskell Setup.hs build
    $> runhaskell Setup.hs test
    $> runhaskell Setup.hs haddock --hyperlink-source
    $> runhaskell Setup.hs copy
    $> runhaskell Setup.hs register

The test step is optional and currently does nothing. The Haddock
step is also optional.


## Links

* [Website](http://wrengr.org/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/stm-chans)
* [GitHub](https://github.com/wrengr/stm-chans)
