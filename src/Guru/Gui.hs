module Guru.Gui
  ( Gui
  , build
  , enterConnectedState
  , enterDisconnectedState
  , connectMsgSubmitted

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
  ) where

import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gtk as Gtk

import qualified Guru.Gui.Gdb as GdbW
import qualified Guru.Gui.Threads as ThreadsW
import Types

data Gui = Gui
  { _gdb_w     :: GdbW.GdbW
  , _threads_w :: ThreadsW.ThreadsW
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

    -- Create the GDB widget
    gdb_w <- GdbW.build
    gdb_w' <- GdbW.getGtkWidget gdb_w
    Gtk.panedPack1 horiz gdb_w' True True

    -- Create the threads widget
    threads_w <- ThreadsW.build
    threads_w' <- ThreadsW.getGtkWidget threads_w
    Gtk.panedPack2 horiz threads_w' False True

    #showAll w

    return (Gui gdb_w threads_w)

enterConnectedState :: Gui -> IO ()
enterConnectedState = GdbW.enterConnectedState . _gdb_w

enterDisconnectedState :: Gui -> IO ()
enterDisconnectedState = GdbW.enterConnectedState . _gdb_w

--------------------------------------------------------------------------------
-- * Signals

-- | Signalled when a non-empty text is entered in the GDB widget's entry.
connectMsgSubmitted :: Gui -> (T.Text -> IO ()) -> IO ()
connectMsgSubmitted = GdbW.connectMsgSubmitted . _gdb_w

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
