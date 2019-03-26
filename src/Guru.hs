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
    gdb <- Gdb.spawn gdb_args (handleGdbMsg gui) (handleGdbStderr gui) (addIdle . Gui.addRawOutMsg gui) (handleGdbExit gui)
    Gui.enterConnectedState gui
    Gui.connectMsgSubmitted gui (msgSubmitted gui gdb)
    Gui.connectGetExprChildren gui (getExprChildren gui gdb)
    Gui.connectExprAdded gui (addExpr gui gdb)

addIdle :: IO () -> IO ()
addIdle f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT_IDLE (f >> return False))

handleGdbStderr :: Gui -> T.Text -> IO ()
handleGdbStderr gui = addIdle . Gui.addStderrMsg gui

handleGdbExit :: Gui -> IO ()
handleGdbExit = addIdle . Gui.enterDisconnectedState

msgSubmitted :: Gui -> Gdb -> T.Text -> IO ()
msgSubmitted gui gdb msg = do
    Gdb.sendRawMsg gdb msg
    -- addIdle (Gui.addUserMsg gui msg)

handleGdbMsg :: Gui -> Gdb -> Gdb.ResultOrOOB -> IO ()
handleGdbMsg gui gdb msg =
    case msg of
      Gdb.OOB (Gdb.ExecAsyncRecord async) -> do
        addIdle $ Gui.addExecMsg gui (renderAsyncRecord async)
        handleAsyncMsg async
      Gdb.OOB (Gdb.StatusAsyncRecord async) -> do
        addIdle $ Gui.addStatusMsg gui (renderAsyncRecord async)
        handleAsyncMsg async
      Gdb.OOB (Gdb.NotifyAsyncRecord async) -> do
        addIdle $ Gui.addNotifyMsg gui (renderAsyncRecord async)
        handleAsyncMsg async
      Gdb.OOB (Gdb.ConsoleStreamRecord msg') ->
        addIdle $ Gui.addConsoleStreamMsg gui msg'
      Gdb.OOB (Gdb.TargetStreamRecord msg') ->
        addIdle $ Gui.addTargetStreamMsg gui msg'
      Gdb.OOB (Gdb.LogStreamRecord msg') ->
        addIdle $ Gui.addLogStreamMsg gui msg'
      Gdb.Result cls vars ->
        addIdle $ Gui.addResultMsg gui (T.pack (show cls)) (renderVarList (M.toList vars))
  where
    handleAsyncMsg :: Gdb.AsyncRecord -> IO ()
    handleAsyncMsg (Gdb.AsyncRecord cls res) = case cls of
      -- Breakpoint messages: refresh breakpoints
      "breakpoint-created" -> handleBpMsg res
      "breakpoint-modified" -> handleBpMsg res

      -- Execution stopped, update backtraces and expressions
      "stopped" -> do
        Gdb.getThreadInfo gdb $ \thread_info ->
          forM_ (Gdb._threadInfoThreads thread_info) $ \(Gdb.ThreadInfoThread thread_id target_id) ->
             Gdb.getThreadBacktrace gdb thread_id $
               addIdle . Gui.addThread gui thread_id target_id

      _ -> return ()

    handleBpMsg :: M.Map Gdb.Var Gdb.Val -> IO ()
    handleBpMsg _msg = return ()

getExprChildren :: Gui -> Gdb -> T.Text -> IO ()
getExprChildren gui gdb expr = undefined

addExpr :: Gui -> Gdb -> T.Text -> IO ()
addExpr gui gdb expr = undefined

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
