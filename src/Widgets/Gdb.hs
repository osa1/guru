-- | A test view for showing GDB output + entry for sending raw commands.
module Widgets.Gdb
  ( GdbWidget
  , build
  , getGtkWidget
  , enterConnectedState
  , enterDisconnectedState
  , connectMsgSubmitted
  , addError
  , addParsedMsg
  , addStderrMsg
  , addUserMsg
  ) where

import Control.Monad
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

-- | Layout: expander -> box -> [ scrolled -> text view, entry ]
data GdbWidget = GdbWidget
  { _gdbWidgetExpander :: !Gtk.Expander
  , _gdbWidgetTextView :: !Gtk.TextView
  , _gdbWidgetEntry    :: !Gtk.Entry
  }

build :: IO GdbWidget
build = do
    -- Create widgets
    expander <- new Gtk.Expander [ #label := "GDB", #expanded := True ]
    box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 0 ]
    scrolled <- new Gtk.ScrolledWindow
      [ #hscrollbarPolicy := Gtk.PolicyTypeAutomatic
      , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
      ]
    -- TODO: how to set adjustments?
    text_view <- new Gtk.TextView
      [ #monospace := True
      , #vexpand := True
      , #editable := False
      ]
    entry <- new Gtk.Entry
      [ #vexpand := False
      , #placeholderText := "(enter gdb or gdb-mi commands here)"
      , #sensitive := False
      ]

    -- Add text view to scrolled
    #add scrolled text_view
    -- Add scrolled to box
    Gtk.boxPackStart box scrolled True True 0
    -- Add entry to box
    Gtk.boxPackStart box entry False False 0
    -- Add box to expander
    #add expander box

    return GdbWidget
      { _gdbWidgetExpander = expander
      , _gdbWidgetTextView = text_view
      , _gdbWidgetEntry = entry
      }

getGtkWidget :: GdbWidget -> IO Gtk.Widget
getGtkWidget = Gtk.toWidget . _gdbWidgetExpander

-- | Enables the entry
enterConnectedState :: GdbWidget -> IO ()
enterConnectedState w = set (_gdbWidgetEntry w) [ #sensitive := True ]

-- | Disables the entry, resets the "msg submitted" callback.
enterDisconnectedState :: GdbWidget -> IO ()
enterDisconnectedState GdbWidget{ _gdbWidgetEntry = entry } = do
    set entry [ #sensitive := False ]
    void (on entry #activate (return ()))

connectMsgSubmitted :: GdbWidget -> (T.Text -> IO ()) -> IO ()
connectMsgSubmitted GdbWidget{ _gdbWidgetEntry = entry } cb =
    void $ on entry #activate $ do
      t <- Gtk.entryGetText entry
      unless (T.null t) $ do
        Gtk.entrySetText entry ""
        cb t

addMsg :: GdbWidget -> T.Text -> T.Text -> IO ()
addMsg w pfx msg = do
    buf <- Gtk.textViewGetBuffer (_gdbWidgetTextView w)
    end_iter <- Gtk.textBufferGetEndIter buf
    msg' <- GLib.markupEscapeText msg (-1)
    Gtk.textBufferInsertMarkup buf end_iter (pfx <> msg' <> "\n") (-1)

addError :: GdbWidget -> T.Text -> IO ()
addError w msg = addMsg w "<span color=\"red\">[ERROR]</span> " msg

addParsedMsg :: GdbWidget -> T.Text -> IO ()
addParsedMsg w msg = addMsg w "[PARSED] " msg

addStderrMsg :: GdbWidget -> T.Text -> IO ()
addStderrMsg w msg = addMsg w "<span color=\"red\">[STDERR]</span> " msg

addUserMsg :: GdbWidget -> T.Text -> IO ()
addUserMsg w msg = addMsg w "[USER] " msg
