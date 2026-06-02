-- |
-- Module      : Spec
-- Description : This module contains tests for the `FormulaManipulator` library.
-- Copyright   : Tygo van den Hurk (1705709)
--               Kylian Maas (1712861)
-- Date:       : 2026-06-02
-- License     : None
--

import ExprLanguage (Expr (Const, Mult, Plus, Var), parseExpr)
import FormulaManipulator
  ( diffE,
    evalE,
    foldE,
    normalizeE,
    printE,
    processCLIArgs,
    simplifyE,
  )
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "FormulaManipulator" $ do
    let zero = Const (0 :: Integer) :: Expr String Integer
        one = Const (1 :: Integer) :: Expr String Integer
        two = Const (2 :: Integer) :: Expr String Integer
        three = Const (3 :: Integer) :: Expr String Integer
        four = Const (4 :: Integer) :: Expr String Integer
        six = Const (6 :: Integer) :: Expr String Integer
        x = Var ("x" :: String) :: Expr String Integer
        y = Var ("y" :: String) :: Expr String Integer

        varDef :: String -> Integer
        varDef "x" = 3
        varDef "y" = 4
        varDef _ = error "Not a defined variable"

        x' = varDef "x"
        y' = varDef "y"

    describe "foldE" $ do

      describe "toString" $ do
        let foldPrint = foldE id show (\l r -> "("++l++"+"++r++")") (\l r -> "("++l++"*"++r++")")
        
        describe "Base Case" $ do
          
          describe "Const" $ do
            it "1 => \"1\"" $ do
              foldPrint one `shouldBe` "1"
          
          describe "Var" $ do
            it "x => \"x\"" $ do
              foldPrint x `shouldBe` "x"

          describe "Plus" $ do
            it "1 + 2 => \"(1+2)\"" $ do
              foldPrint (Plus one two) `shouldBe` "(1+2)"
          
          describe "Mult" $ do
            it "1 + 2 => \"(1+2)\"" $ do
              foldPrint (Mult one two) `shouldBe` "(1*2)"

        describe "Step Case" $ do
          it "1 + 2 * 3 => \"(1+(2*3))\"" $ do
              foldPrint (Plus one (Mult two three)) `shouldBe` "(1+(2*3))"
          it "1 + 2 * x => \"(1+(2*x))\"" $ do
              foldPrint (Plus one (Mult two x)) `shouldBe` "(1+(2*x))"

    describe "printE" $ do
      describe "Base Case" $ do
        describe "Const" $ do
          it "1 = 1" $ do
            printE one `shouldBe` "1"

        describe "Var" $ do
          it "x = x" $ do
            printE x `shouldBe` "x"
          it "y = y" $ do
            printE y `shouldBe` "y"

      describe "Step Case" $ do
        describe "Const" $ do
          it "1 + 1 = 1 + 1" $ do
            printE (Plus one one) `shouldBe` "1 + 1"
          it "1 * 1 = 1 * 1" $ do
            printE (Mult one one) `shouldBe` "1 * 1"
          it "(2 * 3) + 1 != 2 * (3 + 1)" $ do
            parseExpr "2 * 3 + 1" `shouldBe` Right (Plus (Mult two three) one)
            printE (Plus (Mult two three) one) `shouldBe` "2 * 3 + 1"
            parseExpr "2 * (3 + 1)" `shouldBe` Right (Mult two (Plus three one))
            printE (Mult two (Plus three one)) `shouldBe` "2 * (3 + 1)"
          it "(2 * 3) + x != 2 * (3 + x)" $ do
            parseExpr "2 * 3 + x" `shouldBe` Right (Plus (Mult two three) x)
            printE (Plus (Mult two three) x) `shouldBe` "2 * 3 + x"
            parseExpr "2 * (3 + x)" `shouldBe` Right (Mult two (Plus three x))
            printE (Mult two (Plus three x)) `shouldBe` "2 * (3 + x)"

          it "(2 + x) * (2 + y) != 2 + (x * 2) + y" $ do
            parseExpr "(2 + x) * (2 + y)" `shouldBe` Right (Mult (Plus two x) (Plus two y))
            printE (Mult (Plus two x) (Plus two y)) `shouldBe` "(2 + x) * (2 + y)"
            parseExpr "2 + x * 2 + y" `shouldBe` Right (Plus (Plus two (Mult x two)) y)
            printE (Plus (Plus two (Mult x two)) y) `shouldBe` "2 + x * 2 + y"
            printE (Plus two (Plus (Mult x two) y)) `shouldBe` "2 + x * 2 + y"

    describe "evalE" $ do
      let eval = evalE varDef

      describe "Base Case" $ do
        describe "Const" $ do
          it "1 = 1" $ do
            eval one `shouldBe` 1

        describe "Var" $ do
          it ("x = " ++ show x' ++ " for x = " ++ show x') $ do
            eval x `shouldBe` x'
          it ("y = " ++ show y' ++ " for y = " ++ show y') $ do
            eval y `shouldBe` y'

      describe "Step Case" $ do
        describe "Const" $ do
          it "1 + 1 = 2" $ do
            eval (Plus one one) `shouldBe` 2

        describe "Var" $ do
          it ("x + 1 = " ++ show (x' + 1) ++ " for x = " ++ show x') $ do
            eval (Plus x one) `shouldBe` x' + 1
            eval (Plus one x) `shouldBe` x' + 1
          it ("y * 3 = " ++ show (y' * 3) ++ " for y = " ++ show y') $ do
            eval (Mult y three) `shouldBe` y' * 3
            eval (Mult three y) `shouldBe` y' * 3
          it ("(x + y) * 2 = " ++ show ((y' + x') * 2) ++ " for (x,y) = " ++ show (x', y')) $ do
            eval (Plus x y) `shouldBe` (y' + x')
            eval (Mult (Plus x y) two) `shouldBe` (y' + x') * 2
            eval (Mult two (Plus x y)) `shouldBe` (y' + x') * 2

    describe "simplifyE" $ do
      describe "Base Case" $ do
        describe "Const" $ do
          it "1 = 1" $ do
            simplifyE one `shouldBe` one

        describe "Var" $ do
          it "x = x" $ do
            simplifyE x `shouldBe` x

      describe "Step Case" $ do
        describe "Plus" $ do
          describe "Const" $ do
            it "1 + 1 = 2" $ do
              simplifyE (Plus one one) `shouldBe` two
            it "1 + 2 = 3" $ do
              simplifyE (Plus one two) `shouldBe` three
            it "1 + 1 + 1 = 3" $ do
              simplifyE (Plus one (Plus one one)) `shouldBe` three
              simplifyE (Plus (Plus one one) one) `shouldBe` three

          describe "Var" $ do
            it "0 + (x + 1) = x + 1" $ do
              simplifyE (Plus zero (Plus x one)) `shouldBe` Plus x one
            it "1 + (x + 0) = 1 + x" $ do
              simplifyE (Plus one (Plus x zero)) `shouldBe` Plus one x
            it "0 + (x + 0) = x" $ do
              simplifyE (Plus zero (Plus x zero)) `shouldBe` x
            it "x + x = 2 * x" $ do
              simplifyE (Plus x x) `shouldBe` Mult two x
            it "x + y != 2 * x" $ do
              simplifyE (Plus x y) `shouldBe` Plus x y
              simplifyE (Plus y x) `shouldBe` Plus y x
            it "x + 2 * x = 3 * x" $ do
              simplifyE (Plus x (Mult two x)) `shouldBe` Mult three x
              simplifyE (Plus x (Mult x two)) `shouldBe` Mult three x
            it "x + x + x = 3 * x" $ do
              simplifyE (Plus x (Plus x x)) `shouldBe` Mult three x
              simplifyE (Plus (Plus x x) x) `shouldBe` Mult three x
            it "2 * x + 2 * x = 4 * x" $ do
              simplifyE (Plus (Mult two x) (Mult two x)) `shouldBe` Mult four x
              simplifyE (Plus (Mult x two) (Mult two x)) `shouldBe` Mult four x
              simplifyE (Plus (Mult two x) (Mult x two)) `shouldBe` Mult four x
              simplifyE (Plus (Mult x two) (Mult x two)) `shouldBe` Mult four x
            it "x + 3 * x = 4 * x" $ do
              simplifyE (Plus x (Mult x three)) `shouldBe` Mult four x
              simplifyE (Plus (Mult x three) x) `shouldBe` Mult four x
              simplifyE (Plus (Mult three x) x) `shouldBe` Mult four x
              simplifyE (Plus x (Mult three x)) `shouldBe` Mult four x
            it "x + x + x + x = 4 * x" $ do
              simplifyE (Plus (Plus x x) (Plus x x)) `shouldBe` Mult four x

        describe "Mult" $ do
          describe "Const" $ do
            it "1 * 3 = 3" $ do
              simplifyE (Mult one three) `shouldBe` three
              simplifyE (Mult three one) `shouldBe` three
            it "2 * 3 = 6" $ do
              simplifyE (Mult two three) `shouldBe` six
              simplifyE (Mult three two) `shouldBe` six
            it "1 * 2 * 3 = 6" $ do
              simplifyE (Mult one (Mult two three)) `shouldBe` six
              simplifyE (Mult one (Mult three two)) `shouldBe` six
              simplifyE (Mult (Mult three two) one) `shouldBe` six
              simplifyE (Mult (Mult two three) one) `shouldBe` six
            it "0 * 2 * 3 = 6" $ do
              simplifyE (Mult zero (Mult two three)) `shouldBe` zero
              simplifyE (Mult zero (Mult three two)) `shouldBe` zero
              simplifyE (Mult three (Mult zero two)) `shouldBe` zero
              simplifyE (Mult three (Mult two zero)) `shouldBe` zero
              simplifyE (Mult two (Mult zero three)) `shouldBe` zero
              simplifyE (Mult two (Mult three zero)) `shouldBe` zero
              simplifyE (Mult (Mult three two) zero) `shouldBe` zero
              simplifyE (Mult (Mult two three) zero) `shouldBe` zero
              simplifyE (Mult (Mult two zero) three) `shouldBe` zero
              simplifyE (Mult (Mult zero two) three) `shouldBe` zero
              simplifyE (Mult (Mult three zero) two) `shouldBe` zero
              simplifyE (Mult (Mult zero three) two) `shouldBe` zero

          describe "Var" $ do
            it "0 * x = 0" $ do
              simplifyE (Mult zero x) `shouldBe` zero
              simplifyE (Mult x zero) `shouldBe` zero
            it "x * y = x * y" $ do
              simplifyE (Mult x y) `shouldBe` Mult x y
              simplifyE (Mult y x) `shouldBe` Mult y x
            it "0 * x * y = 0" $ do
              simplifyE (Mult zero (Mult y x)) `shouldBe` zero
              simplifyE (Mult zero (Mult x y)) `shouldBe` zero
              simplifyE (Mult y (Mult zero x)) `shouldBe` zero
              simplifyE (Mult y (Mult x zero)) `shouldBe` zero
              simplifyE (Mult x (Mult zero y)) `shouldBe` zero
              simplifyE (Mult x (Mult y zero)) `shouldBe` zero
              simplifyE (Mult (Mult y x) zero) `shouldBe` zero
              simplifyE (Mult (Mult x y) zero) `shouldBe` zero
              simplifyE (Mult (Mult zero x) y) `shouldBe` zero
              simplifyE (Mult (Mult x zero) y) `shouldBe` zero
              simplifyE (Mult (Mult zero y) x) `shouldBe` zero
              simplifyE (Mult (Mult y zero) x) `shouldBe` zero
            it "1 * x * y = x * y" $ do
              simplifyE (Mult one (Mult y x)) `shouldBe` Mult y x
              simplifyE (Mult y (Mult one x)) `shouldBe` Mult y x
              simplifyE (Mult y (Mult x one)) `shouldBe` Mult y x
              simplifyE (Mult (Mult y x) one) `shouldBe` Mult y x
              simplifyE (Mult (Mult one y) x) `shouldBe` Mult y x
              simplifyE (Mult (Mult y one) x) `shouldBe` Mult y x
              simplifyE (Mult one (Mult x y)) `shouldBe` Mult x y
              simplifyE (Mult x (Mult one y)) `shouldBe` Mult x y
              simplifyE (Mult x (Mult y one)) `shouldBe` Mult x y
              simplifyE (Mult (Mult x y) one) `shouldBe` Mult x y
              simplifyE (Mult (Mult one x) y) `shouldBe` Mult x y
              simplifyE (Mult (Mult x one) y) `shouldBe` Mult x y
            it "2 * x * 3 = 6 * x" $ do
              simplifyE (Mult two (Mult three x)) `shouldBe` Mult six x
              simplifyE (Mult three (Mult two x)) `shouldBe` Mult six x
              simplifyE (Mult three (Mult x two)) `shouldBe` Mult six x
              simplifyE (Mult (Mult three x) two) `shouldBe` Mult six x
              simplifyE (Mult two (Mult x three)) `shouldBe` Mult six x
              simplifyE (Mult (Mult x three) two) `shouldBe` Mult six x
              simplifyE (Mult (Mult two x) three) `shouldBe` Mult six x
              simplifyE (Mult (Mult x two) three) `shouldBe` Mult six x

            it "2 * x * 2 + 2 * x = 6 * x" $ do
              simplifyE (Plus (Mult x two) (Mult two (Mult two x))) `shouldBe` Mult six x
              simplifyE (Plus (Mult two x) (Mult two (Mult two x))) `shouldBe` Mult six x
              simplifyE (Plus (Mult two (Mult two x)) (Mult x two)) `shouldBe` Mult six x
              simplifyE (Plus (Mult two (Mult two x)) (Mult two x)) `shouldBe` Mult six x
              simplifyE (Plus (Mult x two) (Mult two (Mult x two))) `shouldBe` Mult six x
              simplifyE (Plus (Mult two x) (Mult two (Mult x two))) `shouldBe` Mult six x
              simplifyE (Plus (Mult two (Mult x two)) (Mult x two)) `shouldBe` Mult six x
              simplifyE (Plus (Mult two (Mult x two)) (Mult two x)) `shouldBe` Mult six x
              simplifyE (Plus (Mult x two) (Mult (Mult two x) two)) `shouldBe` Mult six x
              simplifyE (Plus (Mult two x) (Mult (Mult two x) two)) `shouldBe` Mult six x
              simplifyE (Plus (Mult (Mult two x) two) (Mult x two)) `shouldBe` Mult six x
              simplifyE (Plus (Mult (Mult two x) two) (Mult two x)) `shouldBe` Mult six x
              simplifyE (Plus (Mult x two) (Mult (Mult x two) two)) `shouldBe` Mult six x
              simplifyE (Plus (Mult two x) (Mult (Mult x two) two)) `shouldBe` Mult six x
              simplifyE (Plus (Mult (Mult x two) two) (Mult x two)) `shouldBe` Mult six x
              simplifyE (Plus (Mult (Mult x two) two) (Mult two x)) `shouldBe` Mult six x
              
        describe "diffE" $ do
          let diff = diffE "x"

          describe "Base Case" $ do
            describe "Const" $ do
              it "1' = 0 when diff on x" $ do
                diff one `shouldBe` zero

            describe "Var" $ do
              it "x' = 1 when diff on x" $ do
                diff x `shouldBe` one
              it "y' = 0 when diff on x" $ do
                diff y `shouldBe` zero

          describe "Step Case" $ do
            describe "Const" $ do
              it "(1 + 1)' = 0 when diff on x" $ do
                diff (Plus one one) `shouldBe` zero
              it "(1 * 3)' = 0 when diff on x" $ do
                diff (Mult one three) `shouldBe` zero
                diff (Mult three one) `shouldBe` zero

            describe "Var" $ do
              it "(1 + x)' = 1 when diff on x" $ do
                diff (Plus one x) `shouldBe` one
              it "(3 * x)' = 3 when diff on x" $ do
                diff (Mult x three) `shouldBe` three
                diff (Mult three x) `shouldBe` three
              it "(3 * x + 1)' = 3 when diff on x" $ do
                diff (Plus one (Mult x three)) `shouldBe` three
                diff (Plus (Mult three x) one) `shouldBe` three
              it "(3 * x + 2 * y)' = 3 when diff on x" $ do
                diff (Plus (Mult two y) (Mult x three)) `shouldBe` three
                diff (Plus (Mult two y) (Mult three x)) `shouldBe` three
                diff (Plus (Mult y two) (Mult x three)) `shouldBe` three
                diff (Plus (Mult y two) (Mult three x)) `shouldBe` three
                diff (Plus (Mult three x) (Mult two y)) `shouldBe` three
                diff (Plus (Mult x three) (Mult two y)) `shouldBe` three
                diff (Plus (Mult three x) (Mult y two)) `shouldBe` three
                diff (Plus (Mult x three) (Mult y two)) `shouldBe` three
              it "(3 * x * x)' = 6 * x when diff on x" $ do
                diff (Mult (Mult x x) three) `shouldBe` Mult six x
                diff (Mult three (Mult x x)) `shouldBe` Mult six x
              it "(3 * x * x + 2 * x + 3)' = 6 * x + 2 when diff on x" $ do
                diff (Plus (Plus (Mult (Mult x x) three) (Mult two x)) three) `shouldBe` Plus (Mult six x) two
                diff (Plus (Plus (Mult three (Mult x x)) (Mult two x)) three) `shouldBe` Plus (Mult six x) two
                diff (Plus (Plus (Mult (Mult x x) three) (Mult x two)) three) `shouldBe` Plus (Mult six x) two
                diff (Plus (Plus (Mult three (Mult x x)) (Mult x two)) three) `shouldBe` Plus (Mult six x) two
                diff (Plus three (Plus (Mult (Mult x x) three) (Mult two x))) `shouldBe` Plus (Mult six x) two
                diff (Plus three (Plus (Mult three (Mult x x)) (Mult two x))) `shouldBe` Plus (Mult six x) two
                diff (Plus three (Plus (Mult (Mult x x) three) (Mult x two))) `shouldBe` Plus (Mult six x) two
                diff (Plus three (Plus (Mult three (Mult x x)) (Mult x two))) `shouldBe` Plus (Mult six x) two
              it "(2 * x + 3 * x * x + 3)' = 2 + 6 * x when diff on x" $ do
                diff (Plus (Plus (Mult two x) (Mult (Mult x x) three) ) three) `shouldBe` Plus two (Mult six x)
                diff (Plus (Plus (Mult two x) (Mult three (Mult x x)) ) three) `shouldBe` Plus two (Mult six x)
                diff (Plus (Plus (Mult x two) (Mult (Mult x x) three) ) three) `shouldBe` Plus two (Mult six x)
                diff (Plus (Plus (Mult x two) (Mult three (Mult x x)) ) three) `shouldBe` Plus two (Mult six x)
                diff (Plus three (Plus (Mult two x) (Mult (Mult x x) three) )) `shouldBe` Plus two (Mult six x)
                diff (Plus three (Plus (Mult two x) (Mult three (Mult x x)) )) `shouldBe` Plus two (Mult six x)
                diff (Plus three (Plus (Mult x two) (Mult (Mult x x) three) )) `shouldBe` Plus two (Mult six x)
                diff (Plus three (Plus (Mult x two) (Mult three (Mult x x)) )) `shouldBe` Plus two (Mult six x)

  describe "FormulatorCLI" $ do
    describe "processCLIArgs" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

  -- Bonus exercise tests can be added here
  describe "normalizeE" $ do
    it "should have tests" $ do
      (1 :: Integer) `shouldBe` (1 :: Integer)
