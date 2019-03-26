-- | A tree widget for showing expressions.
module Guru.Gui.Expressions
  ( ExprW
  , build
  , getGtkWidget
  , connectGetChildren
  , connectExprAdded
  ) where

import Control.Monad
import Data.IORef
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gtk as Gtk

-- | Layout: box -> [scrolled -> tree view, entry]
data ExprW = ExprW
  { _exprWStore       :: !Gtk.TreeStore
  , _exprWView        :: !Gtk.TreeView
  , _exprWScrolled    :: !Gtk.ScrolledWindow
  , _exprWEntry       :: !Gtk.Entry
    -- | The top level
  , _exprWBox         :: !Gtk.Box
    -- | Top-level expressions
  , _exprWExprs       :: IORef [Expr]
    -- | How to get children of an expression. `Text` is the full name of the
    -- expression.
  , _exprWGetChildren :: !(IORef (Maybe (T.Text -> IO ())))
    -- | Called when an expression is added.
  , _exprWAdded       :: !(IORef (Maybe (T.Text -> IO ())))
  }

data Expr = Expr
  { -- | Location of this node in the tree.
    _exprIter     :: !Gtk.TreeIter
    -- | Full name of the expression. Not rendered, passed to callbacks to
    -- update state. E.g. "x.y.z"
  , _exprFullName :: !T.Text
    -- | Name of the current node in the parent. E.g. "y" when this is the node
    -- "x.y". Not rendered.
  , _exprName     :: !T.Text
    -- | The expression. TODO: Wat? How is this used? Document this better.
  , _exprExpr     :: !T.Text
  , _exprValue    :: !(Maybe T.Text)
  , _exprType     :: !(Maybe T.Text)
  , _exprChildren :: ![Expr]
  }

colTypes :: [GType]
colTypes =
  [ gtypeString -- full name (not rendered)
  , gtypeString -- expression
  , gtypeString -- value
  , gtypeString -- type
  ]

build :: IO ExprW
build = do
    store <- new Gtk.TreeStore []
    Gtk.treeStoreSetColumnTypes store colTypes -- TODO: How do I do this on init?

    -- TODO: Check that store has ITERS_PERSIST prop set, otherwise we can't
    -- store iters in Exprs

    box <- new Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #spacing := 0
      , #baselinePosition := Gtk.BaselinePositionTop
      ]

    scrolled <- new Gtk.ScrolledWindow
      -- TODO: These props are already defaults so remove this?
      [ #hscrollbarPolicy := Gtk.PolicyTypeAutomatic
      , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
      ]
    Gtk.boxPackStart box scrolled True True 0

    view <- new Gtk.TreeView
      [ #model := store
      , #headersVisible := True
      ]
    #add scrolled view

    -- TODO: How to make this monospace
    entry <- new Gtk.Entry
      [ #placeholderText := "(add expression)"
      ]
    Gtk.boxPackEnd box entry False False 0

    ---------------------------------
    -- Layout done, create columns --
    ---------------------------------

    let add_text_col title col = do
          renderer <- new Gtk.CellRendererText []
          column <- new Gtk.TreeViewColumn [ #title := title ]
          Gtk.treeViewColumnPackStart column renderer True
          Gtk.treeViewColumnAddAttribute column renderer "text" col
          void (Gtk.treeViewAppendColumn view column)

    -- Note that we don't add a col for index 0 here as we don't render the full
    -- name
    add_text_col "Expression" 1
    add_text_col "Value" 2
    add_text_col "Type" 3

    -----------------------------------
    -- Connect "row expanded" signal --
    -----------------------------------

    -- If a row was expanded before then we don't do anything. If it's being
    -- expanded for the first time, we query child nodes.

    get_children_ref <- newIORef Nothing
    exprs_ref <- newIORef []

    void $ Gtk.onTreeViewRowExpanded view $ \_iter path -> do
      mb_get_children <- readIORef get_children_ref
      forM_ mb_get_children $ \get_children ->
        -- These indices give the location of the node in the tree.
        -- TODO: Somehow check this?
        Gtk.treePathGetIndices path >>= \case
          Nothing -> putStrLn "Empty expression index"
          Just [] -> putStrLn "Empty expression index"
          Just (i0 : is) -> do
            exprs <- readIORef exprs_ref
            let top_expr = exprs !! fromIntegral i0
            let expr = foldr (\i e -> _exprChildren e !! fromIntegral i) top_expr is
            when (null (_exprChildren expr)) (get_children (_exprFullName expr))

    ---------------------------------------
    -- Connect "expression added" signal --
    ---------------------------------------

    expr_added_ref <- newIORef Nothing

    void $ on entry #activate $ do
      t <- Gtk.entryGetText entry
      unless (T.null t) $ do
        Gtk.entrySetText entry ""
        readIORef expr_added_ref >>= mapM_ ($ t)

    return (ExprW store view scrolled entry box exprs_ref get_children_ref expr_added_ref)

getGtkWidget :: ExprW -> IO Gtk.Widget
getGtkWidget = Gtk.toWidget . _exprWBox

connectGetChildren :: ExprW -> (T.Text -> IO ()) -> IO ()
connectGetChildren w cb = writeIORef (_exprWGetChildren w) (Just cb)

connectExprAdded :: ExprW -> (T.Text -> IO ()) -> IO ()
connectExprAdded w cb = writeIORef (_exprWAdded w) (Just cb)
