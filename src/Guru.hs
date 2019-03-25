module Guru (run) where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Guru.Gdb (Gdb)
import qualified Guru.Gdb as Gdb
import Guru.Gui (Gui)
import qualified Guru.Gui as Gui

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
    gdb <- Gdb.spawn gdb_args (handleGdbMsg gui) (handleGdbStderr gui) (handleGdbExit gui)
    Gui.connectMsgSubmitted gui (msgSubmitted gui gdb)

addIdle :: IO () -> IO ()
addIdle f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT_IDLE (f >> return False))

handleGdbStderr :: Gui -> T.Text -> IO ()
handleGdbStderr gui = addIdle . Gui.addStderrMsg gui

handleGdbExit :: Gui -> IO ()
handleGdbExit = Gui.enterDisconnectedState

msgSubmitted :: Gui -> Gdb -> T.Text -> IO ()
msgSubmitted gui gdb msg = do
    Gdb.sendRawMsg gdb msg
    Gui.addUserMsg gui msg

handleGdbMsg :: Gui -> Gdb -> Gdb.Out -> IO ()
handleGdbMsg w _gdb (Gdb.Out _token msg) =
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
      "breakpoint-created" -> handleBpMsg res
      "breakpoint-modified" -> handleBpMsg res

      -- Execution stopped, update backtraces and expressions
      "stopped" ->

        return ()

      _ -> return ()

    handleBpMsg :: M.Map Gdb.Var Gdb.Val -> IO ()
    handleBpMsg _msg = return ()

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
