module Test.Language.CarneadesIntoDung.Translation (tests) where

import Data.Either (lefts, rights)
import Data.Set (fromList)
import Test.Tasty
import Test.Tasty.HUnit

import Language.Dung.AF (DungAF(..), groundedExt)
import Language.Carneades.CarneadesDSL (CAES(..), mkProp, getAllArgs, applicable,
                                        getProps, acceptable)
import Language.Carneades.ExampleCAES (caes)
import Language.CarneadesIntoDung.Translation

tests :: TestTree
tests = testGroup "Language.CarneadesIntoDung.Translation"
  [ translateTests
  , groundedExtTests
  , correspondenceTests
  ]

translateTests :: TestTree
translateTests = testGroup "translate"
  [ testCase "translate produces a non-empty AF" $ do
      let AF args attacks = translate caes
      assertBool "AF should have arguments" (not (null args))
      assertBool "AF should have attacks" (not (null attacks))
  , testCase "translate' produces a non-empty labelled AF" $ do
      let AF args attacks = translate' caes
      assertBool "labelled AF should have arguments" (not (null args))
      assertBool "labelled AF should have attacks" (not (null attacks))
  , testCase "defeater is in the grounded extension" $ do
      let ext = groundedExt (translate caes)
      assertBool "defeater should be in grounded extension"
        (Left (mkProp "defeater") `elem` ext)
  , testCase "assumptions are in the grounded extension" $ do
      let CAES (_, (assumptions, _), _) = caes
      let ext = lefts $ groundedExt (translate caes)
      mapM_ (\a -> assertBool ("assumption " ++ show a ++ " should be in extension")
                              (a `elem` ext))
            assumptions
  ]

groundedExtTests :: TestTree
groundedExtTests = testGroup "groundedExt"
  [ testCase "grounded extension contains applicable arguments" $ do
      let CAES (argSet, _, _) = caes
      let appArgs = filter (`applicable` caes) (getAllArgs argSet)
      let ext = rights $ groundedExt (translate caes)
      mapM_ (\a -> assertBool ("applicable arg " ++ show a ++ " should be in extension")
                              (a `elem` ext))
            appArgs
  , testCase "grounded extension contains acceptable propositions" $ do
      let CAES (argSet, (assumptions, _), _) = caes
      let accProps = filter (\p -> p `acceptable` caes || p `elem` assumptions)
                            (getProps argSet)
      let ext = lefts $ filter (/= Left (mkProp "defeater"))
                                (groundedExt (translate caes))
      assertEqual "acceptable propositions should match"
        (fromList accProps) (fromList ext)
  ]

correspondenceTests :: TestTree
correspondenceTests = testGroup "correspondence"
  [ testCase "corApp caes returns True" $
      corApp caes @?= True
  , testCase "corAcc caes returns True" $
      corAcc caes @?= True
  , testCase "corProp (combined) returns True" $ do
      let result = corApp caes && corAcc caes
      result @?= True
  ]
