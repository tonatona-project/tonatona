module Main where

import RIO
import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest

main :: IO ()
main = do
  doctest $ flags ++ pkgs ++ module_sources
