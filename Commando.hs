{-# LANGUAGE OverloadedStrings #-}

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
main = getArgs >>= options False False

options :: Bool -> Bool -> [String] -> IO ()
options _ _ ("-h"        : _) = help
options _ _ ("--help"    : _) = help
options _ c ("-s"        :xs) = options True c    xs
options _ c ("--silent"  :xs) = options True c    xs
options s _ ("-c"        :xs) = options s    True xs
options s _ ("--consumer":xs) = options s    True xs
options s c ("--"        :xs) = command s    c    xs
options s c xs                = command s    c    xs

command :: Bool -> Bool -> [String] -> IO ()
command s c [cmd]      = getD >>= start s c cmd
command s c [cmd, dir] =          start s c cmd (mkPath dir)
command _ _ _          = help

help :: IO ()
help = putStrLn "Usage: commando [--help | -h] [--silent | -s] [--consumer | -c] [--] <command> [directory]"

mkPath :: String -> FilePath
mkPath = fromText . pack

getD :: IO FilePath
getD = getWorkingDirectory

ite :: Bool -> t -> t -> t
ite b t f = if b then t else f

start :: Bool -> Bool -> String -> FilePath -> IO ()
start silent consumer cmd dir = do
  man <- startManager
  watchTree man dir (const True) (ite consumer (void . rawSystem cmd . return . show)
                                               (const $ void $ rawSystem cmd []))
  when (not silent) $ putStrLn "press retrun to stop"
  void $ getLine
  void $ stopManager man
