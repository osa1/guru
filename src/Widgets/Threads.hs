-- | A scrolled window of backtraces.
module Widgets.Threads
  ( ThreadsW
  , build
  , getGtkWidget
  , addThread
  , resetCols
  ) where

import Control.Monad
import Data.IORef
import Data.List
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Types
import qualified Widgets.Backtrace as BtW

-- | Layout: scrolled -> box -> [expander -> BacktraceW]
data ThreadsW = ThreadsW
  { _threadsWScrolledWindow :: Gtk.ScrolledWindow
  , _threadsWBox            :: Gtk.Box
  , _threadsWThreads        :: IORef [BtW.BacktraceW]
  }

build :: IO ThreadsW
build = do
    scrolled <- new Gtk.ScrolledWindow
      [ #hscrollbarPolicy := Gtk.PolicyTypeAutomatic
      , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
      ]

    box <- new Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #spacing := 0
      , #baselinePosition := Gtk.BaselinePositionTop
      ]

    #add scrolled box

    threads <- newIORef []

    return (ThreadsW scrolled box threads)

getGtkWidget :: ThreadsW -> IO Gtk.Widget
getGtkWidget = Gtk.toWidget . _threadsWScrolledWindow

addThread :: ThreadsW -> ThreadId -> TargetId -> Backtrace -> IO ()
addThread w thread_id target_id bt = do
    expander <- new Gtk.Expander
      [ #label := "#" <> T.pack (show thread_id) <> " " <> target_id
      , #expanded := True
      , #vexpand := False
      ]

    bt_w <- BtW.build bt
    BtW.getGtkWidget bt_w >>= #add expander

    Gtk.boxPackStart (_threadsWBox w) expander False False 0
    modifyIORef (_threadsWThreads w) (bt_w :)

    #showAll (_threadsWBox w)

-- | This makes the column widths of all backtrace widgets the same. Kind of a
-- hack to make the whole thing look like one tree view.
resetCols :: ThreadsW -> IO ()
resetCols w = do
    bt_ws <- readIORef (_threadsWThreads w)
    col_ws <- mapM BtW.getColWidths bt_ws
    let max_cols =
          -- Drop the last column with `init`: it should take rest of the space
          init (foldl1' (zipWith max) col_ws)
    forM_ bt_ws $ \bt_w -> BtW.setColWidths bt_w max_cols
