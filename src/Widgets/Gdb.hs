-- | A test view for showing GDB output + entry for sending raw commands.
module Widgets.Gdb
  ( GdbWidget
  , build
  , getGtkWidget
  , enterConnectedState
  , enterDisconnectedState
  , addError
  , addParsedMsg
  , addStderrMsg
  ) where

import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib

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

-- | Disables the entry
enterDisconnectedState :: GdbWidget -> IO ()
enterDisconnectedState w = set (_gdbWidgetEntry w) [ #sensitive := False ]

addMsg :: GdbWidget -> T.Text -> T.Text -> IO ()
addMsg w pfx msg = do
    buf <- Gtk.textViewGetBuffer (_gdbWidgetTextView w)
    end_iter <- Gtk.textBufferGetEndIter buf
    msg' <- GLib.markupEscapeText msg (-1)
    Gtk.textBufferInsertMarkup buf end_iter (pfx <> msg' <> "\n") (-1)

errorPfx :: T.Text
errorPfx = "<span color=\"red\">[ERROR]</span> "

addError :: GdbWidget -> T.Text -> IO ()
addError w msg = addMsg w errorPfx msg

parsedPfx :: T.Text
parsedPfx = "[PARSED] "

addParsedMsg :: GdbWidget -> T.Text -> IO ()
addParsedMsg w msg = addMsg w parsedPfx msg

stderrPfx :: T.Text
stderrPfx = "<span color=\"red\">[STDERR]</span> "

addStderrMsg :: GdbWidget -> T.Text -> IO ()
addStderrMsg w msg = addMsg w stderrPfx msg
