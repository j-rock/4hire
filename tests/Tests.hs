{-# LANGUAGE TemplateHaskell #-}

module Main where


import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.HUnit
--import           Test.QuickCheck.Modifiers

main :: IO ()
main = $(defaultMainGenerator)


type Prop a = a -> Bool

prop_true :: Prop ()
prop_true _ = True

case_tcase :: Assertion
case_tcase = assertBool "Hi" True
