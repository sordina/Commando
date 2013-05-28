{-# LANGUAGE OverloadedStrings #-}

-- TODO: Allow command to be passed arguments from output of Commando

module Main where

import Prelude hiding            (FilePath)
import Control.Monad             (void, when)
import System.Process            (rawSystem)
import System.Environment        (getArgs)
import System.FSNotify           (startManager, watchTree, stopManager)
import Filesystem                (getWorkingDirectory)
import Filesystem.Path.CurrentOS (FilePath, fromText)
import Data.Text                 (pack)

main :: IO ()
main = getArgs >>= processOptions False False

processOptions :: Bool -> Bool -> [String] -> IO ()
processOptions _ _ ("-h"        : _) = help
processOptions _ _ ("--help"    : _) = help
processOptions _ c ("-s"        :xs) = processOptions True c    xs
processOptions _ c ("--silent"  :xs) = processOptions True c    xs
processOptions s _ ("-c"        :xs) = processOptions s    True xs
processOptions s _ ("--consumer":xs) = processOptions s    True xs
processOptions s c ("--"        :xs) = processCommand s    c    xs
processOptions s c xs                = processCommand s    c    xs

processCommand :: Bool -> Bool -> [String] -> IO ()
processCommand s c [command]      = getD >>= start s c command
processCommand s c [command, dir] =          start s c command (mkPath dir)
processCommand _ _ _              = help

help :: IO ()
help = putStrLn "Usage: commando [--help | -h] [--silent | -s] [--consumer | -c] [--] <command> [directory]"

mkPath :: String -> FilePath
mkPath = fromText . pack

getD :: IO FilePath
getD = getWorkingDirectory

ite :: Bool -> t -> t -> t
ite b t f = if b then t else f

start :: Bool -> Bool -> String -> FilePath -> IO ()
start silent consumer command dir = do
  man <- startManager
  watchTree man dir (const True) (ite consumer (void . rawSystem command . return . show)
                                               (const $ void $ rawSystem command []))
  when (not silent) $ putStrLn "press retrun to stop"
  void $ getLine
  void $ stopManager man
