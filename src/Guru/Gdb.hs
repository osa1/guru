-- | GDB bridge. All communication with GDB processes done via this module.
module Guru.Gdb
  ( module Gdb.Syntax
  , Gdb
  , GdbStdoutHandler
  , GdbStderrHandler
  , spawn
  , sendRawMsg
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.Process.Typed

import qualified Gdb.Parser as Gdb
import Gdb.Syntax
import qualified Gdb.Syntax as Gdb

import Prelude hiding (log)

{-
data Gdb = Gdb
  { _gdbProc :: Process Handle Handle Handle
  , _gdbStdoutListener :: ThreadId
  , _gdbStderrListener :: ThreadId
  }
-}

newtype Gdb = Gdb { _gdbStdin :: Handle }

type GdbStdoutHandler = Gdb -> Gdb.Out -> IO ()
type GdbStderrHandler = T.Text -> IO ()

-- | Spawns a GDB process and listens stdout and stderr. Does not block.
spawn
    :: [String]
       -- ^ GDB args. Passed to gdb process as `--args`.
    -> GdbStdoutHandler
       -- ^ GDB message callback.
    -> GdbStderrHandler
       -- ^ GDB bridge logs are passed to this callback.
    -> IO ()
       -- ^ Called when the GDB process exits.
    -> IO Gdb
spawn args handle_msg log exit_cb = do
    let p = setStdin createPipe $
            setStdout createPipe $
            setStderr createPipe $
            proc "gdb" (["-n", "-i=mi", "--args"] <> args)

    stdin_ref <- newEmptyMVar

    void $ forkIO ((withProcess_ p (handleProc stdin_ref) `finally` exit_cb) `catch` exit_code_handler)

    Gdb <$> takeMVar stdin_ref
  where
    handleProc stdin_ref p = do
      putMVar stdin_ref (getStdin p)
      _ <- forkIO (listenStdout (getStdout p) (Gdb (getStdin p)) handle_msg)
      listenStderr (getStderr p) log

    exit_code_handler :: ExitCodeException -> IO ()
    exit_code_handler e = log ("GDB returned non-0: " <> T.pack (show e))

listenStdout :: Handle -> Gdb -> GdbStdoutHandler -> IO ()
listenStdout h gdb cb = loop (parse mempty)
  where
    parse :: BS.ByteString -> A.Result [Gdb.Out]
    parse = A.parse Gdb.parse

    loop (A.Fail unconsumed _ctx err) = do
      hPutStrLn stderr err
      hPutStrLn stderr ("Unconsumed: " ++ show unconsumed)

    loop (A.Partial cont) = do
      bs <- BS.hGetSome h 10000
      -- putStrLn ("Read: " ++ show bs)
      loop (cont bs)

    loop (A.Done unconsumed ret) = do
      -- TODO: Maybe make the cb take [Out] instead of Out? (less idleAdd)
      forM_ ret (cb gdb)
      loop (parse unconsumed)

listenStderr :: Handle -> GdbStderrHandler -> IO ()
listenStderr h cb = loop
  where
    -- TODO: When does this terminate?
    loop = do
      err <- T.hGetLine h
      cb err
      loop

sendRawMsg :: Gdb -> T.Text -> IO ()
sendRawMsg gdb msg = do
    T.hPutStrLn (_gdbStdin gdb) msg
    hFlush (_gdbStdin gdb)
