module Main (main) where

import RIO
import Distribution.Extra.Doctest ( defaultMainWithDoctests )

main :: IO ()
main = defaultMainWithDoctests "doctests"
