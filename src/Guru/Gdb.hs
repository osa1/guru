-- | GDB bridge. All communication with GDB processes done via this module.
module Guru.Gdb
  ( module Gdb.Syntax
  , Gdb
  , GdbStdoutHandler
  , GdbStderrHandler
  , spawn
  , sendRawMsg
  , getThreadInfo
  , getThreadBacktrace
  , getExprChildren

    -- * Message types
  , ThreadInfo (..)
  , ThreadInfoThread (..)
  , ChildrenList (..)
  , Value (..)
  , Backtrace
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.Process.Typed

import Gdb.Messages (Backtrace, ChildrenList (..), ThreadInfo (..),
                     ThreadInfoThread (..), Value (..))
import qualified Gdb.Messages as Gdb
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

data Gdb = Gdb
  { _gdbStdin     :: !Handle
  , _gdbToken     :: !(IORef Int)
  , _gdbCallbacks :: !(IORef (IM.IntMap (Gdb.ResultOrOOB -> IO ())))
  , _gdbLogOut    :: !GdbLogRawMsg
  }

type GdbStdoutHandler = Gdb -> Gdb.ResultOrOOB -> IO ()
type GdbStderrHandler = T.Text -> IO ()
type GdbLogRawMsg = T.Text -> IO ()

-- | Spawns a GDB process and listens stdout and stderr. Does not block.
spawn
    :: [String]
       -- ^ GDB args. Passed to gdb process as `--args`.
    -> GdbStdoutHandler
       -- ^ GDB message callback.
    -> GdbStderrHandler
       -- ^ GDB bridge logs are passed to this callback.
    -> GdbLogRawMsg
       -- ^ How to log outgoing messages
    -> IO ()
       -- ^ Called when the GDB process exits.
    -> IO Gdb
spawn args handle_msg log_stderr log_out exit_cb = do
    let p = setStdin createPipe $
            setStdout createPipe $
            setStderr createPipe $
            proc "gdb" (["-n", "-i=mi", "--args"] <> args)

    stdin_ref <- newEmptyMVar
    token_ref <- newIORef 0
    cbs_ref <- newIORef IM.empty

    void $ forkIO ((withProcess_ p (handleProc stdin_ref token_ref cbs_ref) `finally` exit_cb) `catch` exit_code_handler)

    Gdb <$> takeMVar stdin_ref <*> pure token_ref <*> pure cbs_ref <*> pure log_out
  where
    handleProc stdin_ref token_ref cbs_ref p = do
      putMVar stdin_ref (getStdin p)
      let gdb = Gdb (getStdin p) token_ref cbs_ref log_out
      _ <- forkIO (listenStdout (getStdout p) gdb handle_msg)
      listenStderr (getStderr p) log_stderr

    exit_code_handler :: ExitCodeException -> IO ()
    exit_code_handler e = log_stderr ("GDB returned non-0: " <> T.pack (show e))

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
      forM_ ret $ \out@(Gdb.Out mb_token msg) ->
        case mb_token of
          Nothing -> cb gdb msg
          Just t ->
            atomicModifyIORef' (_gdbCallbacks gdb) (\cbs -> (IM.delete t cbs, IM.lookup t cbs)) >>= \case
              Nothing -> hPutStrLn stderr ("Can't find callback for msg: " ++ show out)
              Just t_cb -> t_cb msg
      loop (parse unconsumed)

listenStderr :: Handle -> GdbStderrHandler -> IO ()
listenStderr h cb = loop
  where
    -- TODO: When does this terminate?
    loop = do
      err <- T.hGetLine h
      cb err
      loop

getToken :: Gdb -> IO Int
getToken gdb = atomicModifyIORef' (_gdbToken gdb) (\t -> (t+1, t))

addCb :: Gdb -> Int -> (Gdb.ResultOrOOB -> IO ()) -> IO ()
addCb gdb t cb = atomicModifyIORef' (_gdbCallbacks gdb) (\cbs -> (IM.insert t cb cbs, ()))

--------------------------------------------------------------------------------
-- * Sending messages to GDB

sendRawMsg :: Gdb -> T.Text -> IO ()
sendRawMsg gdb msg = do
    T.hPutStrLn (_gdbStdin gdb) msg
    hFlush (_gdbStdin gdb)
    _gdbLogOut gdb msg

-- | Request thread info from GDB. Sends a `-thread-info` command.
getThreadInfo :: Gdb -> (ThreadInfo -> IO ()) -> IO ()
getThreadInfo gdb cb = do
    t <- getToken gdb
    addCb gdb t (handleThreadInfoRet cb)
    sendRawMsg gdb (T.pack (show t) <> "-thread-info")

-- | Request backtrace of thread with the given thread id. Sends a
-- `-stack-list-frames` command.
getThreadBacktrace :: Gdb -> Int -> (Backtrace -> IO ()) -> IO ()
getThreadBacktrace gdb thread_id cb = do
    t <- getToken gdb
    addCb gdb t (handleStackListFramesRet cb)
    sendRawMsg gdb (T.pack (show t) <> "-stack-list-frames --thread " <> T.pack (show thread_id))

-- | Requests children of an expression. Sends a `-var-list-children` command.
getExprChildren
    :: Gdb
    -> T.Text -- ^ Full name of the expression
    -> (ChildrenList -> IO ())
    -> IO ()
getExprChildren gdb expr cb = do
    t <- getToken gdb
    addCb gdb t (handleListChildrenRet cb)
    sendRawMsg gdb (T.pack (show t) <> "-var-list-children --all-values " <> expr)

--------------------------------------------------------------------------------
-- * Callback handling

handleCommandRet
    :: String -- ^ Name of the command
    -> (M.Map Gdb.Var Gdb.Val -> Maybe a) -- ^ Parser for the command
    -> (a -> IO ()) -- ^ Callback to be called on successful parse
    -> Gdb.ResultOrOOB -- ^ GDB result
    -> IO ()
handleCommandRet cmd parse cb msg =
    case msg of
      Gdb.Result Gdb.Done vals ->
        case parse vals of
          Nothing -> hPutStrLn stderr ("Can't parse " ++ cmd ++ " ret: " ++ show vals)
          Just ret -> cb ret
      _ -> hPutStrLn stderr ("Unexpected " ++ cmd ++ " result msg: " ++ show msg)

handleThreadInfoRet :: (ThreadInfo -> IO ()) -> Gdb.ResultOrOOB -> IO ()
handleThreadInfoRet = handleCommandRet "thread-info" Gdb.parseThreadInfo

handleStackListFramesRet :: (Backtrace -> IO ()) -> Gdb.ResultOrOOB -> IO ()
handleStackListFramesRet = handleCommandRet "stack-list-frames" Gdb.parseThreadBacktrace

handleListChildrenRet :: (ChildrenList -> IO ()) -> Gdb.ResultOrOOB -> IO ()
handleListChildrenRet = handleCommandRet "var-list-children" Gdb.parseChildrenList
