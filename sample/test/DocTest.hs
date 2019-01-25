module Main (main) where

import Tonalude

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doDocTest

doDocTest :: [String] -> IO ()
doDocTest options =
  doctest $
    options <>
    ghcExtensions

ghcExtensions :: [String]
ghcExtensions =
    [ "-XAutoDeriveTypeable"
    , "-XBangPatterns"
    , "-XBinaryLiterals"
    , "-XConstraintKinds"
    , "-XDataKinds"
    , "-XDefaultSignatures"
    , "-XDeriveDataTypeable"
    , "-XDeriveFoldable"
    , "-XDeriveFunctor"
    , "-XDeriveGeneric"
    , "-XDeriveTraversable"
    , "-XDoAndIfThenElse"
    , "-XDuplicateRecordFields"
    , "-XEmptyDataDecls"
    , "-XExistentialQuantification"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XFunctionalDependencies"
    , "-XGADTs"
    , "-XGeneralizedNewtypeDeriving"
    , "-XInstanceSigs"
    , "-XKindSignatures"
    , "-XLambdaCase"
    , "-XMonadFailDesugaring"
    , "-XMultiParamTypeClasses"
    , "-XMultiWayIf"
    , "-XNamedFieldPuns"
    , "-XNoImplicitPrelude"
    , "-XOverloadedStrings"
    , "-XPartialTypeSignatures"
    , "-XPatternGuards"
    , "-XPolyKinds"
    , "-XRankNTypes"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XStandaloneDeriving"
    , "-XTupleSections"
    , "-XTypeFamilies"
    , "-XTypeSynonymInstances"
    , "-XViewPatterns"
    ]
