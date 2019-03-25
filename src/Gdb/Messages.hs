-- | Parsing GDB syntax to more meaningful messages.
module Gdb.Messages
  ( ThreadInfo (..)
  , ThreadInfoThread (..)
  , parseThreadInfo

  , Backtrace
  , parseThreadBacktrace
  ) where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T

import Gdb.Syntax
import Types

--------------------------------------------------------------------------------

-- | The full message has lots of details, but we only parse what we need.
newtype ThreadInfo = ThreadInfo
  { _threadInfoThreads :: [ThreadInfoThread]
  } deriving (Show)

data ThreadInfoThread = ThreadInfoThread
  { _threadInfoThreadId :: !Int
  , _threadInfoTargetId :: !T.Text
  } deriving (Show)

parseThreadInfo :: M.Map Var Val -> Maybe ThreadInfo
parseThreadInfo vals =
    ThreadInfo <$> (M.lookup "threads" vals >>= preview _ValList >>= mapM parse_thread)
  where
    parse_thread :: Val -> Maybe ThreadInfoThread
    parse_thread val = do
      m <- preview _Tuple val
      ThreadInfoThread
        <$> (M.lookup "id" m >>= preview _Const >>= pure . read . T.unpack)
        <*> (M.lookup "target-id" m >>= preview _Const)

--------------------------------------------------------------------------------

parseThreadBacktrace :: M.Map Var Val -> Maybe Backtrace
parseThreadBacktrace vals =
    M.lookup "stack" vals >>= preview _ResList >>= mapM parse_thread
  where
    parse_thread :: (Var, Val) -> Maybe Frame
    parse_thread (var, val) = do
      guard (var == "frame")
      m <- preview _Tuple val
      Frame <$> (M.lookup "level" m >>= preview _Const >>= pure . read . T.unpack)
            <*> (M.lookup "addr" m >>= preview _Const)
            <*> (M.lookup "func" m >>= preview _Const)
            <*> pure (M.lookup "file" m >>= preview _Const)
            <*> pure (M.lookup "fullname" m >>= preview _Const)
            <*> pure (fmap (read . T.unpack) (M.lookup "line" m >>= preview _Const))
            <*> pure Nothing -- TODO
