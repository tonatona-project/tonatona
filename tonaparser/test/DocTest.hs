
module Main (main) where

import Data.Semigroup ((<>))
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doDocTest

doDocTest :: [String] -> IO ()
doDocTest options =
  doctest $
    options <>
    ghcExtensions <>
    -- This was added because doctest was segfaulting:
    -- https://github.com/sol/doctest/issues/162
    ["-v"]

ghcExtensions :: [String]
ghcExtensions =
    [ "-XConstraintKinds"
    , "-XDataKinds"
    , "-XDeriveDataTypeable"
    , "-XDeriveFunctor"
    , "-XDeriveGeneric"
    , "-XDuplicateRecordFields"
    , "-XEmptyDataDecls"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGADTs"
    , "-XGeneralizedNewtypeDeriving"
    , "-XInstanceSigs"
    , "-XLambdaCase"
    , "-XMultiParamTypeClasses"
    , "-XNamedFieldPuns"
    , "-XNoImplicitPrelude"
    , "-XNoMonomorphismRestriction"
    , "-XOverloadedLabels"
    , "-XOverloadedLists"
    , "-XOverloadedStrings"
    , "-XPackageImports"
    , "-XPatternSynonyms"
    , "-XPolyKinds"
    , "-XRankNTypes"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XStandaloneDeriving"
    , "-XTupleSections"
    , "-XTypeApplications"
    , "-XTypeFamilies"
    , "-XTypeOperators"
    , "-XViewPatterns"
    ]
