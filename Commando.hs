{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import Prelude hiding            (FilePath)
import Control.Monad             (void, when)
import System.Process            (rawSystem, runCommand, runInteractiveCommand)
import System.FSNotify           (startManager, watchTree, stopManager)
import Filesystem                (getWorkingDirectory)
import Filesystem.Path.CurrentOS (FilePath, fromText)
import Data.Text                 (pack)
import System.IO                 (hPutStrLn, hGetContents, hSetBuffering, BufferMode(..), stdout, hIsEOF)
import GHC.IO.Handle             (hClose, hFlush)
import GHC.IO.Handle.Types       (Handle)
import System.Process.Internals  (ProcessHandle)
import Control.Applicative       ((<$>), (<*>))
import Data.Monoid               (mempty, (<>))

import qualified Options.Applicative as O

type RunningProcess = (Handle, Handle, Handle, ProcessHandle)

data Options = Options { command'   :: String
                       , quiet      :: Bool
                       , consumer   :: Bool
                       , stdin      :: Bool
                       , persist    :: Bool
                       , directory' :: Maybe String
                       } deriving Show

options :: O.Parser Options
options = Options <$>             O.argument O.str ( O.metavar "COMMAND"              <> O.help "Command run on events")
                  <*>             O.switch         ( O.short 'q' <> O.long "quiet"    <> O.help "Hide non-essential output")
                  <*>             O.switch         ( O.short 'c' <> O.long "consumer" <> O.help "Pass events as argument to command")
                  <*>             O.switch         ( O.short 'i' <> O.long "stdin"    <> O.help "Pipe events to command")
                  <*>             O.switch         ( O.short 'p' <> O.long "persist"  <> O.help "Pipe events to persistent command")
                  <*> O.optional (O.argument O.str ( O.metavar "DIRECTORY"            <> O.help "Directory to monitor" ))

parser :: O.ParserInfo Options
parser = O.info (O.helper <*> options) mempty

main :: IO ()
main = O.execParser parser >>= command

command :: Options -> IO ()
command o@(directory' -> Just d) = start o (fromText $ pack d)
command o                        = getWorkingDirectory >>= start o

start :: Options -> FilePath -> IO ()
start o dir = do
  man <- startManager
  rc  <- if persist o then Just <$> startPipe (command' o)
                      else return Nothing

  when (not $ quiet o) $ putStrLn "press retrun to stop"

  let cmd = command' o

  void $ watchTree man dir (const True)
       $ case (consumer o, stdin o || persist o )
           of (True      , _    ) -> void . rawSystem cmd . return . show
              (_         , True ) -> void . pipe rc cmd . show
              (_         , _    ) -> const $ void $ runCommand cmd

  void $ getLine

  void $ stopManager man

  case rc of Just p -> pipeClose p
             _      -> return ()

startPipe :: String -> IO RunningProcess
startPipe cmd = do
  rc@(hStdIn,_,_,_) <- runInteractiveCommand cmd
  hSetBuffering hStdIn NoBuffering
  return rc

pipe :: Maybe RunningProcess -> String -> String -> IO ()
pipe Nothing   cmd arg = startPipe cmd >>= pipeRunning arg >>= pipeClose
pipe (Just rp) _   arg = void $ pipeRunning arg rp

pipeClose :: RunningProcess -> IO ()
pipeClose (hStdIn, _, _, _) = hClose hStdIn

pipeRunning :: String -> RunningProcess -> IO RunningProcess
pipeRunning param rc@(hStdIn, hStdOut, _stderr, _process) = do

  hPutStrLn hStdIn param
  hFlush hStdIn

  eof <- hIsEOF hStdOut

  when (not eof) $ do

    hGetContents hStdOut >>= putStr
    hFlush stdout

  return rc
