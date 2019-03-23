module Main where

import System.Environment (getArgs)
import qualified Guru

main :: IO ()
main = getArgs >>= Guru.run
