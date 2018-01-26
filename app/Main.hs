module Main where

import Lib
import System.TimeIt


main :: IO ()
main = do
  timeIt $ print "Haha"
