module Main where

import           SynthesisMainUtil
import           System.Environment

main = do
  args <- getArgs
  case args of
    []    -> putStrLn "Err: file name is not secified"
    _:_:_ -> putStrLn "Err: the number of file names must be one."
    [fp]  -> synthesis fp
