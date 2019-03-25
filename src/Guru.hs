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
import qualified Widgets.Gdb as GdbW
import qualified Widgets.Threads as ThreadsW

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
    w <- new Gtk.ApplicationWindow
      [ #application := app
      , #title := "Guru"
      , #defaultHeight := 200
      , #defaultWidth := 200
      ]

    box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 0 ]
    #add w box

    -- Create the GDB widget
    gdb_w <- GdbW.build
    gdb_w' <- GdbW.getGtkWidget gdb_w
    Gtk.boxPackStart box gdb_w' True True 0

    -- Create the threads widget
    threads_w <- ThreadsW.build
    threads_w' <- ThreadsW.getGtkWidget threads_w
    Gtk.boxPackStart box threads_w' True True 0

    #showAll w

    _ <- forkIO (runGdb gdb_w gdb_args)

    ThreadsW.addThread threads_w 123 "Some target"
      []
    ThreadsW.addThread threads_w 456 "Some other target"
      []

    return ()

addIdle :: IO () -> IO ()
addIdle f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT_IDLE (f >> return False))

-- | Runs a GDB process. DOES NOT fork a thread, but updates widgets with
-- `addIdle`.
runGdb :: GdbW.GdbW -> [String] -> IO ()
runGdb gdb_w gdb_args = do
    GdbW.enterConnectedState gdb_w

    let
      p = setStdin createPipe $
          setStdout createPipe $
          setStderr createPipe $
          proc "gdb" (["-n", "-i=mi", "--args"] <> gdb_args)

      after_p = addIdle (GdbW.enterDisconnectedState gdb_w)
      exit_code_handler (e :: ExitCodeException) = addIdle (GdbW.addError gdb_w (T.pack (show e)))

      go p_ = do
        GdbW.connectMsgSubmitted gdb_w (msgSubmitted gdb_w (getStdin p_))
        runGdbProcess p_ gdb_w

    (withProcess_ p go `finally` after_p) `catch` exit_code_handler

msgSubmitted :: GdbW.GdbW -> Handle -> T.Text -> IO ()
msgSubmitted w h t = do
    putStrLn ("msgSubmitted: " ++ show t)
    T.hPutStrLn h t
    hFlush h
    GdbW.addUserMsg w t

runGdbProcess :: Process Handle Handle Handle -> GdbW.GdbW -> IO ()
runGdbProcess p w = do
    _ <- forkIO (gdbStderrListener (getStderr p) w `finally` putStrLn "stderr listener returned")
    gdbStdoutListener (getStdout p) w `finally` putStrLn "stdout listener returned"

gdbStdoutListener :: Handle -> GdbW.GdbW -> IO ()
gdbStdoutListener h w = loop (parse mempty)
  where
    parse :: BS.ByteString -> A.Result [Gdb.Out]
    parse = A.parse Gdb.parse

    loop (A.Fail _unconsumed _ctx err) = do
      addIdle (GdbW.addError w (T.pack err))
      addIdle (GdbW.addError w (T.pack (show _unconsumed)))

    loop (A.Partial cont) = do
      bs <- BS.hGetSome h 10000
      -- putStrLn ("Read: " ++ show bs)
      loop (cont bs)

    loop (A.Done unconsumed ret) = do
      addIdle (forM_ ret (handleGdbMsg w))
      loop (parse unconsumed)

handleGdbMsg :: GdbW.GdbW -> Gdb.Out -> IO ()
handleGdbMsg w (Gdb.Out _token msg) =
    case msg of
      Gdb.OOB (Gdb.ExecAsyncRecord async) -> do
        GdbW.addExecMsg w (renderAsyncRecord async)
        handleAsyncMsg async
      Gdb.OOB (Gdb.StatusAsyncRecord async) -> do
        GdbW.addStatusMsg w (renderAsyncRecord async)
        handleAsyncMsg async
      Gdb.OOB (Gdb.NotifyAsyncRecord async) -> do
        GdbW.addNotifyMsg w (renderAsyncRecord async)
        handleAsyncMsg async
      Gdb.OOB (Gdb.ConsoleStreamRecord msg') ->
        GdbW.addConsoleStreamMsg w msg'
      Gdb.OOB (Gdb.TargetStreamRecord msg') ->
        GdbW.addTargetStreamMsg w msg'
      Gdb.OOB (Gdb.LogStreamRecord msg') ->
        GdbW.addLogStreamMsg w msg'
      Gdb.Result cls vars ->
        GdbW.addResultMsg w (T.pack (show cls)) (renderVarList (M.toList vars))
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

gdbStderrListener :: Handle -> GdbW.GdbW -> IO ()
gdbStderrListener h w = loop
  where
    -- TODO: When does this terminate?
    loop = do
      err <- T.hGetLine h
      addIdle (GdbW.addStderrMsg w err)
      loop
