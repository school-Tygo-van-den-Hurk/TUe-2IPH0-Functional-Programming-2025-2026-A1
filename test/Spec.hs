{-|
Module      : Spec
Copyright   : STUDENT NAME 1 (ID)
              STUDENT NAME 2 (ID)

This module contains tests for the `FormulaManipulator` library.
-}

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Control.Exception              ( evaluate, try, catch, SomeException)
import           ExprLanguage                   ( Expr(Var, Const, Plus, Mult)
                                                , parseExpr
                                                )
import           FormulaManipulator             ( foldE
                                                , printE
                                                , evalE
                                                , simplifyE
                                                , diffE
                                                , processCLIArgs
                                                , toExprF
                                                , fromExprF
                                                , normalizeE
                                                , ExprF(..)
                                                , Fix(..)
                                                )
import Data.Either                              (fromRight)

main :: IO ()
main = hspec $ do
  describe "FormulaManipulator" $ do

    describe "foldE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "printE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "evalE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "simplifyE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "diffE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "FormulatorCLI" $ do
    describe "processCLIArgs" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

  -- Bonus exercise tests can be added here
  describe "normalizeE" $ do
    it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)