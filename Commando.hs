{-# LANGUAGE OverloadedStrings #-}

-- TODO: Allow command to be passed arguments from output of Commando

module Main where

import Prelude hiding            (FilePath)
import Control.Monad             (void)
import System.Process            (runCommand)
import System.Environment        (getArgs)
import System.FSNotify           (startManager, watchTree, stopManager)
import Filesystem                (getWorkingDirectory)
import Filesystem.Path.CurrentOS (FilePath, fromText)
import Data.Text                 (pack)

main :: IO ()
main = getArgs >>= processArgs

processArgs :: [String] -> IO ()
processArgs []                  = help
processArgs xs | hasHelpFlag xs = help
processArgs [command]           = getWorkingDirectory >>= processCommand command
processArgs [command, dir]      = processCommand command (mkPath dir)
processArgs _                   = help

help :: IO ()
help = putStrLn "Usage: commando <command> [directory]"

hasHelpFlag :: [String] -> Bool
hasHelpFlag xs = any (flip elem xs) ["-h", "--help"]

processCommand :: String -> FilePath -> IO ()
processCommand command dir = do
  man <- startManager
  watchTree man dir (const True) (const $ void $ runCommand command)
  putStrLn "press retrun to stop"
  void $ getLine
  void $ stopManager man

mkPath :: String -> FilePath
mkPath = fromText . pack
