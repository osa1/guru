-- | A list view to show backtraces. This widget can't be updated -- destroy and
-- build again to update.
module Guru.Gui.Backtrace
  ( BacktraceW
  , build
  , getGtkWidget
  , getColWidths
  , setColWidths
  ) where

import Control.Monad
import qualified Data.Text as T
import Data.Int

import Data.GI.Base
import qualified GI.Gtk as Gtk

import Types (Backtrace, Frame (..))

-- | Layout: just a TreeView
data BacktraceW = BacktraceW
  { _backtraceWModel :: Gtk.ListStore
  , _backtraceWView  :: Gtk.TreeView
  }

colTypes :: [GType]
colTypes =
  [ gtypeString -- level
  , gtypeString -- address
  , gtypeString -- function
  , gtypeString -- location
  ]

build :: Backtrace -> IO BacktraceW
build bt = do
    model <- new Gtk.ListStore [] -- [ #columnTypes := colTypes ]
    Gtk.listStoreSetColumnTypes model colTypes -- TODO: Can't do this in the initializer?

    view <- new Gtk.TreeView
      [ #model := model
      , #vexpand := False
      , #hexpand := True
      , #headersVisible := True -- TODO should be False, True for testing purposes
      ]

    let add_text_col title col selectable = do
          renderer <- new Gtk.CellRendererText []
          column <- new Gtk.TreeViewColumn [ #title := title ]
          Gtk.treeViewColumnPackStart column renderer True
          Gtk.treeViewColumnAddAttribute column renderer "text" col
          when selectable $ do
            -- We don't want to allow editing but we want to allow copying the
            -- contents, so we enable editing, but we don't update the text in
            -- "edited" callback.
            Gtk.setCellRendererTextEditable renderer True
            void (on renderer #edited (\_path _text -> return ()))
          void (Gtk.treeViewAppendColumn view column)

    add_text_col "Level" 0 False
    add_text_col "Address" 1 True
    add_text_col "Function" 2 True
    add_text_col "Location" 3 True

    -- Add frames
    forM_ bt $ \frame -> do
      let file_name = case (_frameFile frame, _frameLine frame) of
            (Just file, Just line) -> file <> ":" <> T.pack (show line)
            _ -> ""

      append_iter <- Gtk.listStoreAppend model
      lvl <- toGValue (Just ('#' : show (_frameLevel frame)))
      addr <- toGValue (Just (_frameAddr frame))
      func <- toGValue (Just (_frameFunc frame))
      file_name' <- toGValue (Just file_name)
      Gtk.listStoreSet model append_iter [0,1,2,3] [lvl,addr,func,file_name']

    return (BacktraceW model view)

getGtkWidget :: BacktraceW -> IO Gtk.Widget
getGtkWidget = Gtk.toWidget . _backtraceWView

getColWidths :: BacktraceW -> IO [Int32]
getColWidths bt_w = Gtk.treeViewGetColumns (_backtraceWView bt_w) >>= mapM Gtk.treeViewColumnGetWidth

setColWidths :: BacktraceW -> [Int32] -> IO ()
setColWidths bt_w ws = do
    cols <- Gtk.treeViewGetColumns (_backtraceWView bt_w)
    -- Note that we deliberately accept different lengths here: we want to leave
    -- the last column alone (it should take rest of the space after resizing
    -- all other columns).
    zipWithM_ Gtk.treeViewColumnSetFixedWidth cols ws
