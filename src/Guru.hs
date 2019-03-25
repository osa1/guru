module Guru (run) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.Process.Typed

import Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Gdb.Parser as Gdb
import qualified Gdb.Syntax as Gdb
import qualified Guru.Gui as Gui
import Guru.Gui (Gui)

run :: [String] -> IO ()
run gdb_args = do
    app <- new Gtk.Application
      [ #applicationId := "guru.guru"
      , #flags := [ Gio.ApplicationFlagsFlagsNone ]
      ]
    void (on app #activate (activate app gdb_args))
    void (Gio.applicationRun app Nothing)

activate :: Gtk.Application -> [String] -> IO ()
activate app gdb_args = do
    gui <- Gui.build app
    _ <- forkIO (runGdb gui gdb_args)
    return ()

addIdle :: IO () -> IO ()
addIdle f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT_IDLE (f >> return False))

-- | Runs a GDB process. DOES NOT fork a thread, but updates widgets with
-- `addIdle`.
runGdb :: Gui -> [String] -> IO ()
runGdb gui gdb_args = do
    Gui.enterConnectedState gui

    let
      p = setStdin createPipe $
          setStdout createPipe $
          setStderr createPipe $
          proc "gdb" (["-n", "-i=mi", "--args"] <> gdb_args)

      after_p = addIdle (Gui.enterDisconnectedState gui)
      exit_code_handler (e :: ExitCodeException) = addIdle (Gui.addError gui (T.pack (show e)))

      go p_ = do
        Gui.connectMsgSubmitted gui (msgSubmitted gui (getStdin p_))
        runGdbProcess p_ gui

    (withProcess_ p go `finally` after_p) `catch` exit_code_handler

msgSubmitted :: Gui -> Handle -> T.Text -> IO ()
msgSubmitted w h t = do
    putStrLn ("msgSubmitted: " ++ show t)
    T.hPutStrLn h t
    hFlush h
    Gui.addUserMsg w t

runGdbProcess :: Process Handle Handle Handle -> Gui -> IO ()
runGdbProcess p w = do
    _ <- forkIO (gdbStderrListener (getStderr p) w `finally` putStrLn "stderr listener returned")
    gdbStdoutListener (getStdout p) w `finally` putStrLn "stdout listener returned"

gdbStdoutListener :: Handle -> Gui -> IO ()
gdbStdoutListener h w = loop (parse mempty)
  where
    parse :: BS.ByteString -> A.Result [Gdb.Out]
    parse = A.parse Gdb.parse

    loop (A.Fail _unconsumed _ctx err) = do
      addIdle (Gui.addError w (T.pack err))
      addIdle (Gui.addError w (T.pack (show _unconsumed)))

    loop (A.Partial cont) = do
      bs <- BS.hGetSome h 10000
      -- putStrLn ("Read: " ++ show bs)
      loop (cont bs)

    loop (A.Done unconsumed ret) = do
      addIdle (forM_ ret (handleGdbMsg w))
      loop (parse unconsumed)

handleGdbMsg :: Gui -> Gdb.Out -> IO ()
handleGdbMsg w (Gdb.Out _token msg) =
    case msg of
      Gdb.OOB (Gdb.ExecAsyncRecord async) -> do
        Gui.addExecMsg w (renderAsyncRecord async)
        handleAsyncMsg async
      Gdb.OOB (Gdb.StatusAsyncRecord async) -> do
        Gui.addStatusMsg w (renderAsyncRecord async)
        handleAsyncMsg async
      Gdb.OOB (Gdb.NotifyAsyncRecord async) -> do
        Gui.addNotifyMsg w (renderAsyncRecord async)
        handleAsyncMsg async
      Gdb.OOB (Gdb.ConsoleStreamRecord msg') ->
        Gui.addConsoleStreamMsg w msg'
      Gdb.OOB (Gdb.TargetStreamRecord msg') ->
        Gui.addTargetStreamMsg w msg'
      Gdb.OOB (Gdb.LogStreamRecord msg') ->
        Gui.addLogStreamMsg w msg'
      Gdb.Result cls vars ->
        Gui.addResultMsg w (T.pack (show cls)) (renderVarList (M.toList vars))
  where
    handleAsyncMsg :: Gdb.AsyncRecord -> IO ()
    handleAsyncMsg (Gdb.AsyncRecord cls res) = case cls of
      -- Breakpoint messages: refresh breakpoints
      "breakpoint-created" ->
        -- handleBpMsg res
        return ()
      "breakpoint-modified" ->
        -- handleBpMsg res
        return ()

      -- This is where we refresh backtraces and expressions
      "stopped" ->
        -- handleStoppedMsg res
        return ()

      _ -> return ()

renderVarList :: [(Gdb.Var, Gdb.Val)] -> T.Text
-- TODO: Use a builder?
renderVarList = T.intercalate ", " . map (\(var, val) -> var <> "=" <> renderVal val)

renderVal :: Gdb.Val -> T.Text
renderVal (Gdb.Const t) = t
renderVal (Gdb.Tuple vars) = "{" <> renderVarList (M.toList vars) <> "}"
renderVal (Gdb.ValList vals) = "[" <> T.intercalate "," (map renderVal vals) <> "]"
renderVal (Gdb.ResList vars) = "[" <> renderVarList vars <> "]"

renderAsyncRecord :: Gdb.AsyncRecord -> T.Text
renderAsyncRecord (Gdb.AsyncRecord cls res)
  | M.null res
  = cls
  | otherwise
  = cls <> ": " <> renderVarList (M.toList res)

gdbStderrListener :: Handle -> Gui -> IO ()
gdbStderrListener h w = loop
  where
    -- TODO: When does this terminate?
    loop = do
      err <- T.hGetLine h
      addIdle (Gui.addStderrMsg w err)
      loop
