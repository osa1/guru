-- | A tree widget for showing expressions.
module Guru.Gui.Expressions
  ( ExprW
  , build
  , getGtkWidget
  , addExpr
  , connectGetChildren
  , connectExprAdded
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Data.Int
import Data.IORef
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gtk as Gtk

import Types

import Debug.Trace

-- | Layout: box -> [scrolled -> tree view, entry]
data ExprW = ExprW
  { _exprWStore       :: !Gtk.TreeStore
  , _exprWView        :: !Gtk.TreeView
  , _exprWScrolled    :: !Gtk.ScrolledWindow
  , _exprWEntry       :: !Gtk.Entry
    -- | The top level
  , _exprWBox         :: !Gtk.Box
    -- | Top-level expressions. MVar because we do IO (update GUI) when updating
    -- this.
  , _exprWExprs       :: !(MVar [Expr])
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
    exprs_ref <- newMVar []

    void $ Gtk.onTreeViewRowExpanded view $ \_iter path -> do
      mb_get_children <- readIORef get_children_ref
      forM_ mb_get_children $ \get_children ->
        -- These indices give the location of the node in the tree.
        -- TODO: Somehow check this?
        Gtk.treePathGetIndices path >>= \case
          Nothing -> putStrLn "Empty expression index"
          Just [] -> putStrLn "Empty expression index"
          Just (i0 : is) -> do
            exprs <- readMVar exprs_ref
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

--------------------------------------------------------------------------------
-- * Adding and updating expressions
--------------------------------------------------------------------------------

-- TODO: Document this mess. Clarify name/full name confusion.

addExpr :: ExprW -> T.Text -> Value -> IO ()
addExpr w expr value@(Value val name ty n_children) =
    trace ("addExpr " ++ T.unpack expr ++ " name: " ++ T.unpack name) $
    case T.split (== '.') name of
      [] ->
        putStrLn ("Empty path in addExpr for name: " ++ (T.unpack name))
      n : ns ->
        modifyMVar_ (_exprWExprs w) $ \exprs ->
          case ns of
            [] -> do
              -- Top-level expression
              let store = _exprWStore w
              iter <- Gtk.treeStoreAppend store Nothing
              vals <- mkStoreValues expr value
              Gtk.treeStoreSet store iter storeCols vals
              -- Create the placeholder if this has children
              when (n_children /= 0) (addPlaceholder store iter)
              return (Expr iter name n expr (Just val) (Just ty) [] : exprs)
            _ ->
              -- Nested expression. Find top-level expression that this belongs
              -- to and add child expression to it.
              modifyExpr exprs n (\parent -> addChild (_exprWStore w) parent expr value ns)

storeCols :: [Int32]
storeCols = [0,1,2,3]

mkStoreValues :: T.Text -> Value -> IO [GValue]
mkStoreValues expr (Value val full_name ty _n_children) = do
    col0 <- toGValue (Just full_name) -- full name, not rendered
    col1 <- toGValue (Just expr) -- expression
    col2 <- toGValue (Just val) -- value
    col3 <- toGValue (Just ty) -- type
    return [col0, col1, col2, col3]

modifyExpr :: [Expr] -> T.Text -> (Expr -> IO Expr) -> IO [Expr]
-- Couldn't find the top-level. This should be a bug, so just report it.
modifyExpr [] parent _ = do
    putStrLn ("modifyExpr: Couldn't find parent expression: " ++ T.unpack parent)
    return []
-- Search through the list
modifyExpr (e : es) parent modify
  | trace ("modifyExpr checking " ++ T.unpack (_exprName e)) (_exprName e == parent)
  = (: es) <$> modify e
  | otherwise
  = (e :) <$> modifyExpr es parent modify

addChild
    :: Gtk.TreeStore
    -> Expr -- ^ Parent expression
    -> T.Text -- ^ The expression
    -> Value
    -> [T.Text] -- ^ Path to the child from the parent expression.
    -> IO Expr

addChild _ parent expr _ [] = do
    putStrLn ("addChild: empty path for expr: " ++ T.unpack expr)
    return parent

-- Add the expression to the parent
addChild store parent expr value@(Value val full_name ty n_children) [name] = do
    let parent_iter = _exprIter parent
    vals <- mkStoreValues expr value
    -- If the only child is the placeholder, update it
    children_path <- Gtk.treeModelGetPath store parent_iter
    Gtk.treePathDown children_path
    (True, children_iter) <- Gtk.treeModelGetIter store children_path
    child_gval <- Gtk.treeModelGetValue store children_iter 0
    Just child_val :: Maybe T.Text <- fromGValue child_gval
    iter <-
      if child_val == placeholder then do
        -- Update it
        Gtk.treeStoreSet store children_iter storeCols vals
        return children_iter
      else do
        -- Insert a new row
        iter <- Gtk.treeStoreAppend store (Just parent_iter)
        Gtk.treeStoreSet store iter storeCols vals
        return iter

    -- Create the placeholder if this has children
    when (n_children /= 0) (addPlaceholder store iter)
    return (Expr iter full_name name expr (Just val) (Just ty) [])

-- Find new parent, recurse
addChild store parent expr value (n1:n2:ns) = do
    new_children <- modifyExpr (_exprChildren parent) n1 $ \new_parent -> addChild store new_parent expr value (n2:ns)
    return parent{ _exprChildren = new_children }

addPlaceholder :: Gtk.TreeStore -> Gtk.TreeIter -> IO ()
addPlaceholder store parent_iter =  do
    iter <- Gtk.treeStoreAppend store (Just parent_iter)
    vals <- mkStoreValues placeholder (Value placeholder placeholder placeholder 0)
    Gtk.treeStoreSet store iter storeCols vals

placeholder :: T.Text
placeholder = "__PLACEHOLDER__"

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

connectGetChildren :: ExprW -> (T.Text -> IO ()) -> IO ()
connectGetChildren w cb = writeIORef (_exprWGetChildren w) (Just cb)

connectExprAdded :: ExprW -> (T.Text -> IO ()) -> IO ()
connectExprAdded w cb = writeIORef (_exprWAdded w) (Just cb)
