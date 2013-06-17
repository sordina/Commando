{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding            (FilePath)
import Control.Monad             (void, when)
import System.Process            (rawSystem, runCommand, runInteractiveCommand)
import System.Environment        (getArgs)
import System.FSNotify           (startManager, watchTree, stopManager)
import Filesystem                (getWorkingDirectory)
import Filesystem.Path.CurrentOS (FilePath, fromText)
import Data.Text                 (pack)
import System.IO                 (hPutStrLn, hGetContents, hSetBinaryMode)
import Control.Concurrent        (forkIO)

data Mode = StandAlone
          | Argument
          | StdIn

main :: IO ()
main = getArgs >>= options False StandAlone

options :: Bool -> Mode -> [String] -> IO ()
options _ _ ("-h"        : _) = help
options _ _ ("--help"    : _) = help
options _ c ("-s"        :xs) = options True c        xs
options _ c ("--silent"  :xs) = options True c        xs
options s _ ("-c"        :xs) = options s    Argument xs
options s _ ("--consumer":xs) = options s    Argument xs
options s _ ("-i"        :xs) = options s    StdIn    xs
options s _ ("--stdin"   :xs) = options s    StdIn    xs
options s c ("--"        :xs) = command s    c        xs
options s c xs                = command s    c        xs

command :: Bool -> Mode -> [String] -> IO ()
command s c [cmd]      = getWorkingDirectory >>= start s c cmd
command s c [cmd, dir] =                         start s c cmd (fromText $ pack $ dir)
command _ _ _          = help

help :: IO ()
help = putStrLn "Usage: commando [--help | -h] [--silent | -s] [--consumer | -c] [--] <command> [directory]"

start :: Bool -> Mode -> String -> FilePath -> IO ()
start silent mode cmd dir = do
  man <- startManager
  when (not silent) $ putStrLn "press retrun to stop"
  void $ watchTree man dir (const True)
       $ case mode of StandAlone -> const $ void $ runCommand cmd
                      Argument   -> void . rawSystem cmd . return . show
                      StdIn      -> void . pipe cmd . show
  void $ getLine
  void $ stopManager man

pipe :: String -> String -> IO ()
pipe cmd param = do
  (hStdIn, stdOut, _stderr, _process) <- runInteractiveCommand cmd
  void $ hSetBinaryMode hStdIn False
  void $ forkIO $ hPutStrLn hStdIn param
  void $ forkIO $ hGetContents stdOut >>= putStrLn
