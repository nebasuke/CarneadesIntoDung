module Main (main) where

import Test.Tasty

import qualified Test.Language.CarneadesIntoDung.Translation as Translation

main :: IO ()
main = defaultMain $ testGroup "CarneadesIntoDung"
  [ Translation.tests
  ]
