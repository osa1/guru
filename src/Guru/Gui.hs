module Guru.Gui
  ( Gui
  , build
  , enterConnectedState
  , enterDisconnectedState
  , connectMsgSubmitted
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
  ) where

import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gtk as Gtk

import qualified Guru.Gui.Gdb as GdbW
import qualified Guru.Gui.Threads as ThreadsW

data Gui = Gui
  { _gdb_w     :: GdbW.GdbW
  , _threads_w :: ThreadsW.ThreadsW
  }

build :: Gtk.Application -> IO Gui
build app = do
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
