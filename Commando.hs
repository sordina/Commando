{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import Prelude hiding            (FilePath)
import Control.Monad             (void, when)
import System.Process            (rawSystem, runCommand, runInteractiveCommand)
import System.FSNotify           (startManager, watchTree, stopManager)
import Filesystem.Path.CurrentOS (FilePath, fromText)
import Data.Text                 (pack)
import System.IO                 (hPutStrLn, hGetContents, hSetBuffering, BufferMode(..))
import GHC.IO.Handle             (hClose, hFlush)
import GHC.IO.Handle.Types       (Handle)
import System.Process.Internals  (ProcessHandle)
import Control.Applicative       ((<$>), (<*>))
import Data.Monoid               (mempty, Monoid, (<>))
import Control.Concurrent        (forkIO)
import Data.Maybe                (fromMaybe)

import qualified Options.Applicative.Builder.Internal as X
import qualified Options.Applicative                  as O

type RunningProcess = (Handle, Handle, Handle, ProcessHandle)

data Options = Options { command   :: String
                       , quiet     :: Bool
                       , consumer  :: Bool
                       , stdin     :: Bool
                       , persist   :: Bool
                       , directory :: FilePath
                       } deriving Show

main :: IO ()
main = O.execParser (O.info (O.helper <*> options) mempty) >>= start

options :: O.Parser Options
options = Options
  <$> defStr "echo event" ( O.metavar "COMMAND"              <> O.help "Command run on events")
  <*> O.switch            ( O.short 'q' <> O.long "quiet"    <> O.help "Hide non-essential output")
  <*> O.switch            ( O.short 'c' <> O.long "consumer" <> O.help "Pass events as argument to command")
  <*> O.switch            ( O.short 'i' <> O.long "stdin"    <> O.help "Pipe events to command")
  <*> O.switch            ( O.short 'p' <> O.long "persist"  <> O.help "Pipe events to persistent command")
  <*> (dir <$> defStr "." ( O.metavar "DIRECTORY"            <> O.help "Directory to monitor" ))

defStr :: String -> X.Mod X.ArgumentFields String -> O.Parser String
defStr a = def a . O.argument O.str

def :: a -> O.Parser a -> O.Parser a
def a = fmap (fromMaybe a) . O.optional

dir :: String -> FilePath
dir = fromText . pack

start :: Options -> IO ()
start o = do
  man <- startManager
  rc  <- if persist o then Just <$> startPipe (command o)
                      else return Nothing

  void $ forkIO $ whenM rc $ \(_,so,_,_) -> hGetContents so >>= putStr

  when (not $ quiet o) $ putStrLn "press retrun to stop"

  let cmd = command o

  void $ watchTree man (directory o) (const True)
       $ case (consumer o, stdin o || persist o )
           of (True      , _    ) -> void . rawSystem cmd . return . show
              (_         , True ) -> void . pipe rc cmd . show
              (_         , _    ) -> const $ void $ runCommand cmd

  void $ getLine

  void $ stopManager man

  whenM rc pipeClose

whenM :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenM m f = maybe (return ()) f m

startPipe :: String -> IO RunningProcess
startPipe cmd = do
  rc@(hStdIn,_,_,_) <- runInteractiveCommand cmd
  hSetBuffering hStdIn NoBuffering
  return rc

pipe :: Maybe RunningProcess -> String -> String -> IO ()
pipe Nothing   cmd arg = startPipe cmd >>= pipeSend arg >>= pipeOutput >>= pipeClose
pipe (Just rp) _   arg = void $ pipeSend arg rp

pipeOutput :: RunningProcess -> IO RunningProcess
pipeOutput r@(_,so,_,_) = hGetContents so >>= putStr >> return r

pipeClose :: RunningProcess -> IO ()
pipeClose (hStdIn, _, _, _) = hClose hStdIn

pipeSend :: String -> RunningProcess -> IO RunningProcess
pipeSend param rc@(hStdIn, _hStdOut, _stderr, _process) = do
  hPutStrLn hStdIn param
  hFlush hStdIn
  return rc
