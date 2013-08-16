{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid         (mempty)
import Control.Monad       (when)
import System.Commando     (commando, options, quiet)
import Options.Applicative (info, helper, execParser, (<*>))
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  opts <- execParser (info (helper <*> options) mempty)
  when (not $ quiet opts) $ putStrLn "press return to quit"
  commando opts >>= mapM_ putStr
