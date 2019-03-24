module Gui where

import qualified Widgets.GDb as GdbW

import qualified GI.Gtk as Gtk

data Gui = Gui
  { _guiGdb :: GdbW.GdbWidget
  }
