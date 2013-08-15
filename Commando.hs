{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Commando
import Control.Monad       (when)
import Control.Applicative ((<*>))
import Data.Monoid         (mempty)

import qualified Options.Applicative as O

main :: IO ()
main = do
  opts <- O.execParser (O.info (O.helper <*> options) mempty)

  when (not $ quiet opts) $ putStrLn "press return to stop"

  commando opts >>= mapM_ putStrLn
