{-# LANGUAGE TemplateHaskell #-}

-- | This module defines GDB output syntax.
module Gdb.Syntax where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Text as T

type Var = T.Text

data Val
  = Const !T.Text
  | Tuple !(M.Map Var Val)
  | ValList ![Val]
  | ResList ![(Var, Val)] -- Deliberately NOT a map!
  deriving (Show, Eq)

makePrisms ''Val

data Out = Out
  { _outToken :: !(Maybe Int)
  , _outData  :: !ResultOrOOB
  } deriving (Show, Eq)

data ResultOrOOB
  = Result !ResultClass !(M.Map Var Val)
  | OOB !OOBResult
  deriving (Show, Eq)

data ResultClass
  = Done
  | Running
  | Connected
  | Error
  | Exit
  deriving (Show, Eq)

data OOBResult
  = ExecAsyncRecord !AsyncRecord
  | StatusAsyncRecord !AsyncRecord
  | NotifyAsyncRecord !AsyncRecord
  | ConsoleStreamRecord !T.Text
  | TargetStreamRecord !T.Text
  | LogStreamRecord !T.Text
  deriving (Show, Eq)

data AsyncRecord = AsyncRecord
  { _asyncRecordClass   :: !T.Text -- TODO: This is different than ResultClass?
  , _asyncRecordResults :: !(M.Map Var Val)
  } deriving (Show, Eq)
