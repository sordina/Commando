{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid         (mempty)
import Control.Monad       (when)
import System.Commando     (commando, options, quiet)
import Options.Applicative (info, helper, execParser, (<*>))

main :: IO ()
main = do
  opts <- execParser (info (helper <*> options) mempty)

  when (not $ quiet opts) $ putStrLn "press return to stop"

  commando opts >>= mapM_ putStr
