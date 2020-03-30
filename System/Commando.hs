{-# LANGUAGE OverloadedStrings #-}

-- | A library providing an interface to generate a lazy stream of command
-- results from events occurring in a directory.

module System.Commando (module Data.Default, Options(..), options, commando) where

import Prelude hiding            (FilePath)
import Control.Monad             (void)
import System.Process            (waitForProcess, runInteractiveCommand)
import System.FSNotify           (startManager, watchTree, stopManager, Event(..))
import Filesystem.Path.CurrentOS (FilePath, fromText, encodeString)
import Data.Text                 (pack)
import System.IO                 (hPutStrLn, hGetContents, hSetBuffering, BufferMode(..))
import GHC.IO.Handle             (hClose, hFlush)
import GHC.IO.Handle.Types       (Handle)
import System.Process.Internals  (ProcessHandle)
import Control.Concurrent        (forkIO)
import Data.Maybe                (fromMaybe, isJust, catMaybes)
import Control.Concurrent.Chan   (newChan, writeChan, getChanContents, Chan)

import qualified Options.Applicative.Builder.Internal as X
import qualified Options.Applicative                  as O

import Data.Default

type RunningProcess = (Handle, Handle, Handle, ProcessHandle)

-- | Options used to configure the behavior of Commando

data Options = Options { -- | The commando to run
                         command   :: String
                         -- | Silence any help
                       , quiet     :: Bool
                         -- | Command accepts input as an argument
                       , consumer  :: Bool
                         -- | Command accepts input on STDIN
                       , stdin     :: Bool
                         -- | Command remains running and new events are sent to its STDIN
                       , persist   :: Bool
                         -- | Display show function used to translate events to strings
                       , display   :: Event -> String
                         -- | The directory listened to - Default is the current directory.
                       , directory :: FilePath
                       }

instance Default Options where def = Options "echo" True True False False (show . toFP) "."

-- | The main listening loop.
commando :: Options -> IO [String]
commando o = do
  c <- newChan
  void $ forkIO $ start c o
  catMaybes . takeWhile isJust <$> getChanContents c

-- | An optparse-applicative parser for command-line options.
options :: O.Parser Options
options = Options
  <$> defStr "echo event" ( O.metavar "COMMAND"               <> O.help "Command run on events")
  <*> O.switch            ( O.short 'q' <> O.long "quiet"     <> O.help "Hide non-essential output")
  <*> O.switch            ( O.short 'c' <> O.long "consumer"  <> O.help "Pass events as argument to command")
  <*> O.switch            ( O.short 'i' <> O.long "stdin"     <> O.help "Pipe events to command")
  <*> O.switch            ( O.short 'p' <> O.long "persist"   <> O.help "Pipe events to persistent command")
  <*> ((show <?> toFP) <$>  O.switch ( O.short 'j' <> O.long "path-only" <> O.help "Only show the File-Path, not metadata"))
  <*> (dir <$> defStr "." ( O.metavar "DIRECTORY"             <> O.help "Directory to monitor" ))

defStr :: String -> X.Mod X.ArgumentFields String -> O.Parser String
defStr a = xor a . O.argument O.str

xor :: a -> O.Parser a -> O.Parser a
xor a = fmap (fromMaybe a) . O.optional

dir :: String -> FilePath
dir = fromText . pack

start :: CH -> Options -> IO ()
start c o = do
  man <- startManager
  rc  <- if persist o then Just <$> startPipe (command o)
                      else return Nothing

  void $ forkIO $ whenM rc $ \(_,so,_,_) -> hGetContents so >>= mapM_ (putChan c . (++ "\n")) . lines

  let cmd = command o
      dsp = display o

  void $ watchTree man (encodeString $ directory o) (const True)
       $ case (consumer o, stdin o || persist o )
           of (True      , _    ) -> systemChan c cmd . return . dsp
              (_         , True ) -> void . pipe c rc cmd . dsp
              (_         , _    ) -> const $ systemChan c cmd []

  void getLine
  void $ stopManager man
  whenM rc pipeClose
  closeChan c

systemChan :: CH -> String -> [String] -> IO ()
systemChan c cmd as = do
  (i,o,e,pid) <- runInteractiveCommand (cmd ++ " " ++ (as >>= show))
  hClose i
  hGetContents o >>= putChan c
  hGetContents e >>= putChan c
  void $ waitForProcess pid

whenM :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenM m f = maybe (return ()) f m

startPipe :: String -> IO RunningProcess
startPipe cmd = do
  rc@(hStdIn,_,_,_) <- runInteractiveCommand cmd
  hSetBuffering hStdIn NoBuffering
  return rc

pipe :: CH -> Maybe RunningProcess -> String -> String -> IO ()
pipe c Nothing   cmd arg = startPipe cmd >>= pipeSend arg >>= pipeOutput c >>= pipeClose
pipe _ (Just rp) _   arg = void $ pipeSend arg rp

pipeOutput :: CH -> RunningProcess -> IO RunningProcess
pipeOutput c r@(_,so,_,_) = hGetContents so >>= putChan c >> return r

pipeClose :: RunningProcess -> IO ()
pipeClose (hStdIn, _, _, _) = hClose hStdIn

pipeSend :: String -> RunningProcess -> IO RunningProcess
pipeSend param rc@(hStdIn, _hStdOut, _stderr, _process) = do
  hPutStrLn hStdIn param
  hFlush hStdIn
  return rc

(<?>) :: a -> a -> Bool -> a
x <?> y = \b -> if b then y else x

toFP :: Event -> String
toFP (Added    fp _ _) = fp
toFP (Modified fp _ _) = fp
toFP (Removed  fp _ _) = fp
toFP (Unknown  fp _ _) = fp

-- Chans
type CH = Chan (Maybe String)

putChan :: CH -> String -> IO ()
putChan c = writeChan c . Just

closeChan :: Chan (Maybe a) -> IO ()
closeChan c = writeChan c Nothing
