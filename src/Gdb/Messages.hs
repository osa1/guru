-- | Parsing GDB syntax to more meaningful messages.
module Gdb.Messages
  ( ThreadInfo (..)
  , ThreadInfoThread (..)
  , parseThreadInfo
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Lens

import Gdb.Syntax

-- | The full message has lots of details, but we only parse what we need.
newtype ThreadInfo = ThreadInfo
  { _threadInfoThreads :: [ThreadInfoThread]
  } deriving (Show)

data ThreadInfoThread = ThreadInfoThread
  { _threadInfoThreadId :: !Int
  , _threadInfoTargetId :: !T.Text
  } deriving (Show)

parseThreadInfo :: M.Map Var Val -> Maybe ThreadInfo
parseThreadInfo vals = do
    ThreadInfo <$> (M.lookup "threads" vals >>= preview _ValList >>= mapM parse_thread)
  where
    parse_thread :: Val -> Maybe ThreadInfoThread
    parse_thread val = do
      m <- preview _Tuple val
      ThreadInfoThread
        <$> (M.lookup "id" m >>= preview _Const >>= pure . read . T.unpack)
        <*> (M.lookup "target-id" m >>= preview _Const)
