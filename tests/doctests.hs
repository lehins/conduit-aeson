{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ >= 802
import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "conduit-aeson" =<< getArgs
#else

import System.IO

main :: IO ()
main = hPutStrLn stderr "GHC version older than 8.2 are not supported"

#endif
