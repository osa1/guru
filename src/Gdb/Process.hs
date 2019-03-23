-- | Communication with a GDB process.
module Gdb.Process where

import qualified Data.Text as T
import System.Process.Typed

{-
-- | Forks a gdb process, returns (stdin, stdout, stderr)
spawnGdb
    :: [String] -- ^ Args. Passed to gdb with `--args`
    -> IO ()
spawnGdb gdb_args = do
    let args = ["-n", "-i=mi", "--args"] <> gdb_args
    p <- setStdin createPipe $
         setStdout createPipe $
         setStderr createPipe $
         proc "gdb" args
    return ()
-}
