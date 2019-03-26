module Guru.Gui
  ( Gui
  , build
  , enterConnectedState
  , enterDisconnectedState
  , connectMsgSubmitted
  , connectGetExprChildren
  , connectExprAdded

    -- * Rendering messages
  , addError
  , addParsedMsg
  , addStderrMsg
  , addUserMsg
  , addConsoleStreamMsg
  , addTargetStreamMsg
  , addLogStreamMsg
  , addExecMsg
  , addStatusMsg
  , addNotifyMsg
  , addResultMsg
  , addRawOutMsg

    -- * Thread stuff
  , addThread

    -- * Expressions
  , addExpr

  ) where

import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gtk as Gtk

import qualified Guru.Gui.Expressions as ExprW
import qualified Guru.Gui.Gdb as GdbW
import qualified Guru.Gui.Threads as ThreadsW
import Guru.Types

data Gui = Gui
  { _gdb_w     :: !GdbW.GdbW
  , _threads_w :: !ThreadsW.ThreadsW
  , _expr_w    :: !ExprW.ExprW
  }

build :: Gtk.Application -> IO Gui
build app = do
    w <- new Gtk.ApplicationWindow
      [ #application := app
      , #title := "Guru"
      , #defaultHeight := 800
      , #defaultWidth := 800
      ]

    horiz <- new Gtk.Paned [ #orientation := Gtk.OrientationHorizontal ]
    #add w horiz

    vert <- new Gtk.Paned [ #orientation := Gtk.OrientationVertical ]
    Gtk.panedPack1 horiz vert True True

    -- Create the GDB widget
    gdb_w <- GdbW.build
    gdb_w' <- GdbW.getGtkWidget gdb_w
    Gtk.panedPack1 vert gdb_w' True True

    -- Create the expressions widget
    expr_w <- ExprW.build
    expr_w' <- ExprW.getGtkWidget expr_w
    Gtk.panedPack2 vert expr_w' False True

    -- Create the threads widget
    threads_w <- ThreadsW.build
    threads_w' <- ThreadsW.getGtkWidget threads_w
    Gtk.panedPack2 horiz threads_w' False True

    #showAll w

    return (Gui gdb_w threads_w expr_w)

enterConnectedState :: Gui -> IO ()
enterConnectedState = GdbW.enterConnectedState . _gdb_w

enterDisconnectedState :: Gui -> IO ()
enterDisconnectedState = GdbW.enterConnectedState . _gdb_w

--------------------------------------------------------------------------------
-- * Signals

-- | Signalled when a non-empty text is entered in the GDB widget's entry.
connectMsgSubmitted :: Gui -> (T.Text -> IO ()) -> IO ()
connectMsgSubmitted = GdbW.connectMsgSubmitted . _gdb_w

-- | Register a callback for requesting expression children. `Text` argument is
-- the full name of the expression that we want to ask children of.
connectGetExprChildren :: Gui -> (T.Text -> IO ()) -> IO ()
connectGetExprChildren = ExprW.connectGetChildren . _expr_w

-- | Signalled when a new expression is added via the expression widget's entry.
connectExprAdded :: Gui -> (T.Text -> IO ()) -> IO ()
connectExprAdded = ExprW.connectExprAdded . _expr_w

--------------------------------------------------------------------------------
-- * Rendering GDB messages

addError, addParsedMsg, addStderrMsg, addUserMsg, addConsoleStreamMsg,
  addTargetStreamMsg, addLogStreamMsg, addExecMsg, addStatusMsg, addNotifyMsg
    :: Gui -> T.Text -> IO ()

addError = GdbW.addError . _gdb_w
addParsedMsg = GdbW.addParsedMsg . _gdb_w
addStderrMsg = GdbW.addStderrMsg . _gdb_w
addUserMsg = GdbW.addUserMsg . _gdb_w
addConsoleStreamMsg = GdbW.addConsoleStreamMsg . _gdb_w
addTargetStreamMsg = GdbW.addTargetStreamMsg . _gdb_w
addLogStreamMsg = GdbW.addLogStreamMsg . _gdb_w
addExecMsg = GdbW.addExecMsg . _gdb_w
addStatusMsg = GdbW.addStatusMsg . _gdb_w
addNotifyMsg = GdbW.addNotifyMsg . _gdb_w

addResultMsg
    :: Gui
    -> T.Text -- ^ Class
    -> T.Text -- ^ Msg body. May be empty.
    -> IO ()
addResultMsg = GdbW.addResultMsg . _gdb_w

-- | Log a message from GURU to GDB.
addRawOutMsg :: Gui -> T.Text -> IO ()
addRawOutMsg = GdbW.addRawOutMsg . _gdb_w

--------------------------------------------------------------------------------
-- * Backtrace stuff

addThread :: Gui -> ThreadId -> TargetId -> Backtrace -> IO ()
addThread = ThreadsW.addThread . _threads_w

--------------------------------------------------------------------------------
-- * Expressions

addExpr :: Gui -> T.Text -> Value -> IO ()
addExpr = ExprW.addExpr . _expr_w
