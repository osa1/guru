module Types where

import qualified Data.Text as T
import Data.Word

type Backtrace = [Frame]

data Frame = Frame
  { _frameLevel    :: Word
    -- | The $pc value for the frame.
  , _frameAddr     :: T.Text
    -- | Function name
  , _frameFunc     :: T.Text
    -- | File name of the source file where the function lives.
  , _frameFile     :: Maybe T.Text
    -- | The full file name of the source file where the function lives.
  , _frameFullName :: T.Text
    -- | Line number corresponding to the $pc.
  , _frameLine     :: Maybe Word
    -- | The shared library where this function is defined. This is only given
    -- if the frame’s function is not known.
  , _frameFrom     :: Maybe T.Text
  } deriving (Show)
