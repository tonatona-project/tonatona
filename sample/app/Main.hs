module Main where

import RIO
import Tonatona (run)
import TonaApp.Main (app)

main :: IO ()
main = run app
