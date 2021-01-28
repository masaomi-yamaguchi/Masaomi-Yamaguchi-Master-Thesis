module Main (main) where

import           SynthesisMainUtil
import qualified Criterion.Types as C
import           Criterion.Main

specFile :: String
specFile = "experiments/q2-6/spec.hobit"

conf :: C.Config
conf = defaultConfig -- { C.resamples = 2 }

main :: IO ()
main = defaultMainWith
  conf
  [bgroup "synthesis" [bench specFile $ nfIO (synthesis specFile)]]
