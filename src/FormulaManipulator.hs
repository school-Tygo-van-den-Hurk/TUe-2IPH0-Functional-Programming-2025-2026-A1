{-|
Module      : FormulaManipulator
Description : Manipulate formulas and expressions represented by `Expr` values
Copyright   : STUDENT NAME 1 (ID)
              STUDENT NAME 2 (ID)

`FormulaManipulator` offers functions to manipulate, evaluate, and print
formulas and expressions represented by `Expr` values.
-}

module FormulaManipulator where

import           Data.Either
import           Data.List.Split                (splitOn
                                                , endBy
                                                )
import           Data.Map
import           ExprLanguage                   ( Expr(..)
                                                , parseExpr
                                                , ParseError
                                                )
foldE     = error "Implement, document, and test this function"
printE    = error "Implement, document, and test this function"

evalE     = error "Implement, document, and test this function"
simplifyE = error "Implement, document, and test this function"
diffE     = error "Implement, document, and test this function"
processCLIArgs = error "Implement, document, and test this function"

-- Bonus exercise

newtype Fix f = Fx (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

-- Change this data type accordingly.
data ExprF a b c = ImplementThisDataType

type Algebra f c = f c -> c

cata :: Functor f => Algebra f c -> Fix f -> c 
cata f = f . fmap (cata f) . unFix

printEF = error "Implement, document, and test this function"
evalEF = error "Implement, document, and test this function"
  