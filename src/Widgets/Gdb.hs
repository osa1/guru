-- | A test view for showing GDB output + entry for sending raw commands.
module Widgets.Gdb
  ( GdbW
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
data GdbW = GdbW
  { _gdbWExpander :: !Gtk.Expander
  , _gdbWTextView :: !Gtk.TextView
  , _gdbWEntry    :: !Gtk.Entry
  }

build :: IO GdbW
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

    return GdbW
      { _gdbWExpander = expander
      , _gdbWTextView = text_view
      , _gdbWEntry = entry
      }

getGtkWidget :: GdbW -> IO Gtk.Widget
getGtkWidget = Gtk.toWidget . _gdbWExpander

-- | Enables the entry
enterConnectedState :: GdbW -> IO ()
enterConnectedState w = set (_gdbWEntry w) [ #sensitive := True ]

-- | Disables the entry, resets the "msg submitted" callback.
enterDisconnectedState :: GdbW -> IO ()
enterDisconnectedState GdbW{ _gdbWEntry = entry } = do
    set entry [ #sensitive := False ]
    void (on entry #activate (return ()))

connectMsgSubmitted :: GdbW -> (T.Text -> IO ()) -> IO ()
connectMsgSubmitted GdbW{ _gdbWEntry = entry } cb =
    void $ on entry #activate $ do
      t <- Gtk.entryGetText entry
      unless (T.null t) $ do
        Gtk.entrySetText entry ""
        cb t

addMsg :: GdbW -> T.Text -> T.Text -> IO ()
addMsg w pfx msg = do
    buf <- Gtk.textViewGetBuffer (_gdbWTextView w)
    end_iter <- Gtk.textBufferGetEndIter buf
    msg' <- GLib.markupEscapeText msg (-1)
    Gtk.textBufferInsertMarkup buf end_iter (pfx <> msg' <> "\n") (-1)

addError :: GdbW -> T.Text -> IO ()
addError w msg = addMsg w "<span color=\"red\">[ERROR]</span> " msg

addParsedMsg :: GdbW -> T.Text -> IO ()
addParsedMsg w msg = addMsg w "[PARSED] " msg

addStderrMsg :: GdbW -> T.Text -> IO ()
addStderrMsg w msg = addMsg w "<span color=\"red\">[STDERR]</span> " msg

addUserMsg :: GdbW -> T.Text -> IO ()
addUserMsg w msg = addMsg w "> [USER] " msg
