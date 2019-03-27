-- | Like `ThreadsW`, but has one TreeView.
module Guru.Gui.Threads2 where

import Control.Monad
import Data.Int
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gtk as Gtk

import Guru.Types

-- | Layout: scrolled -> tree view
data ThreadsW = ThreadsW
  { _threadsWScrolledWindow :: !Gtk.ScrolledWindow
  , _threadsWTreeView       :: !Gtk.TreeView
  , _threadsWTreeStore      :: !Gtk.TreeStore
  }

-- | We have 3 layers so column types need to contain columns for all of these:
--
--   - Layer 1: [ thread number, thread name ]
--   - Layer 2: [ level, address, function, location ]
--   - Layer 3: [ variable, type, value ]
--
-- If we want to add more layers in the future (e.g. maybe by expanding
-- expressions) we add more rows of the same type as layer 3.
--
-- Not sure how to best implement this, but I think there are basically two
-- approaches:
--
--   - Use 4 cols, reuse cols for different purposes (e.g. "thread number",
--     "level" and "variable" use the same col in the model)
--   - Use 9 cols.
--
-- I'm currently going for the first approach.
--
-- NOTE: It's important that we keep things in sorted order so that tree path
-- [x, y] gives us x. thread y. frame.
--
colTypes :: [GType]
colTypes =
    [ gtypeString
    , gtypeString
    , gtypeString
    , gtypeString
    , gtypeBoolean -- third column visible?
    , gtypeBoolean -- fourth volumn visible?
    ]

colIndices :: [Int32]
colIndices = [0..5]

build :: IO ThreadsW
build = do
    store <- new Gtk.TreeStore []
    Gtk.treeStoreSetColumnTypes store colTypes -- TODO: How do I do this on init?

    scrolled <- new Gtk.ScrolledWindow
      -- TODO: These props are already defaults so remove this?
      [ #hscrollbarPolicy := Gtk.PolicyTypeAutomatic
      , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
      ]

    view <- new Gtk.TreeView
      [ #model := store
      , #headersVisible := False
      ]
    #add scrolled view

    ---------------------------------
    -- Layout done, create columns --
    ---------------------------------

    let
      add_col
        :: Int32 -- ^ Column index
        -> Maybe Int32 -- ^ Index of the column for this column's visibility
        -> IO ()
      add_col col visibility_col = do
        renderer <- new Gtk.CellRendererText []
        column <- new Gtk.TreeViewColumn []
        Gtk.treeViewColumnPackStart column renderer True
        Gtk.treeViewColumnAddAttribute column renderer "text" col
        forM_ visibility_col $
          Gtk.treeViewColumnAddAttribute column renderer "visible"
        void (Gtk.treeViewAppendColumn view column)

    add_col 0 Nothing
    add_col 1 Nothing
    add_col 2 (Just 4)
    add_col 3 (Just 5)

    -----------------------------------
    -- Connect "row expanded" signal --
    -----------------------------------

    -- TODO

    return (ThreadsW scrolled view store)

getGtkWidget :: ThreadsW -> IO Gtk.Widget
getGtkWidget = Gtk.toWidget . _threadsWScrolledWindow

--------------------------------------------------------------------------------
-- * Adding entries
--------------------------------------------------------------------------------

-- | Make a thread row.
mkLayer0 :: ThreadId -> TargetId -> IO [GValue]
mkLayer0 thread_id target_id = do
    col0 <- toGValue (Just (show thread_id))
    col1 <- toGValue (Just (T.unpack target_id))
    col2 <- toGValue (Nothing :: Maybe String)
    col3 <- toGValue (Nothing :: Maybe String)
    col4 <- toGValue False -- col2 not visible
    col5 <- toGValue False -- col3 not visible
    return [col0,col1,col2,col3,col4,col5]

-- | Make a stack frame row.
mkLayer1 :: Frame -> IO [GValue]
mkLayer1 frame = do
    -- Level
    col0 <- toGValue (Just ('#' : show (_frameLevel frame)))
    -- Address
    col1 <- toGValue (Just (_frameAddr frame))
    -- Function
    col2 <- toGValue (Just (_frameFunc frame))
    -- Location
    let file_name = case (_frameFile frame, _frameLine frame) of
          (Just file, Just line) -> file <> ":" <> T.pack (show line)
          _                      -> ""
    col3 <- toGValue (Just file_name)
    col4 <- toGValue True -- col2 is visible
    col5 <- toGValue True -- col3 is visible
    return [col0,col1,col2,col3,col4,col5]

addThread :: ThreadsW -> ThreadId -> TargetId -> Backtrace -> IO ()
addThread w thread_id target_id bt = do
    let store = _threadsWTreeStore w
    -- Add the top-level (thread)
    top_iter <- Gtk.treeStoreAppend store Nothing
    thread_values <- mkLayer0 thread_id target_id
    Gtk.treeStoreSet store top_iter colIndices thread_values
    -- Add frames
    forM_ bt $ \frame -> do
      frame_values <- mkLayer1 frame
      iter <- Gtk.treeStoreAppend store (Just top_iter)
      Gtk.treeStoreSet store iter colIndices frame_values
    -- Expand the top-level
    top_path <- Gtk.treeModelGetPath store top_iter
    _ <- Gtk.treeViewExpandRow (_threadsWTreeView w) top_path False
    return ()
