-- |
-- Module      : FormulaManipulator
-- Description : Manipulate formulas and expressions represented by `Expr` values
-- Copyright   : Tygo van den Hurk (1705709)
--               Kylian Maas (1712861)
-- Date:       : 2026-06-02
-- License      : None
--
-- `FormulaManipulator` offers functions to manipulate, evaluate, and print
-- formulas and expressions represented by `Expr` values.
module FormulaManipulator where
import Data.Either ()
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import ExprLanguage
  ( Expr (..),
    parseExpr,
  )

-- | Reduces an `Expr` using functions to a value `r` from the bottom up.
--
-- === Examples
--
-- >>> foldE id id (+) (*) (Mult (Const 5) (Const 4))
-- 20
--
-- >>> foldE id id (+) (*) (Plus (Const 5) (Const 4))
-- 9
foldE ::
  -- | Function that converts the value of a `Var` to an `r`.
  (v -> r) ->
  -- | Function that converts the value of a `Const` to an `r`.
  (c -> r) ->
  -- | Function that converts the left and right side of a `Plus` to an `r`.
  (r -> r -> r) ->
  -- | Function that converts the left and right side of a `Mult` to an `r`.
  (r -> r -> r) ->
  -- | The `Expr` to fold
  Expr v c ->
  -- | The final output value
  r
foldE varCase constCase plusCase multCase = rec
  where
    rec (Var variable) = varCase variable
    rec (Const constant) = constCase constant
    rec (Plus expr1 expr2) = plusCase (rec expr1) (rec expr2)
    rec (Mult expr1 expr2) = multCase (rec expr1) (rec expr2)

-- | Pretty prints an `Expr` as a string.
--
-- Takes ambiguity into account using parentheses. I could have done it easily by just using:
--
-- @
-- printE :: Show s => Expr String s -> String
-- printE = foldE id show plusCase multCase
--     where
--         plusCase left right = "(" ++ left ++ " + " ++ right ++ ")"
--         multCase left right = "(" ++ left ++ " * " ++ right ++ ")"
-- @
--
-- But I thought that was lame.
--
-- === Examples
--
-- >>> printE (Const 5)
-- "5"
--
-- >>> printE (Var "x")
-- "x"
--
-- >>> printE (Plus (Var "x") (Const 1))
-- "x + 1"
--
-- >>> printE (Mult (Const 2) (Plus (Var "x") (Const 2)))
-- "2 * (x + 2)"
--
-- >>> printE (Plus (Mult (Const 2) (Var "x")) (Const 2))
-- "2 * x + 2"
--
-- >>> printE (Mult (Plus (Const 2) (Var "x")) (Plus (Const 2) (Var "y")))
-- "(2 + x) * (2 + y)"
printE ::
  (Show s) =>
  -- | The `Expr` to pretty print.
  Expr String s ->
  -- | The resulting string.
  String
printE expr = let (_, result) = rec expr in result
  where
    rec = foldE varCase constCase plusCase multCase
    varCase v = (Var v, v)
    constCase c = (Const c, show c)
    plusCase (l, l') (r, r') = (Plus l r, l' ++ " + " ++ r')
    multCase (l, l') (r, r') = (Mult l r, lhs ++ " * " ++ rhs)
      where
        -- parentheses are only required when pluses are involved
        lhs = case l of
          Plus _ _ -> "(" ++ l' ++ ")"
          _ -> l'
        rhs = case r of
          Plus _ _ -> "(" ++ r' ++ ")"
          _ -> r'

-- | Evaluates an `Expr` to a `Num`.
--
-- === Examples
--
-- >>> evalE (\x -> if x == "x" then 3 else error "not defined") (Const 5)
-- Const 5
--
-- >>> evalE (\x -> if x == "x" then 3 else error "not defined") (Var "x")
-- Const 3
--
-- >>> evalE (\x -> if x == "x" then 3 else error "not defined") (Plus (Var "x") (Const 1))
-- Const 4
--
-- >>> evalE (\x -> if x == "x" then 3 else error "not defined") (Mult (Var "x") (Const 2))
-- Const 6
--
-- >>> evalE (\x -> if x == "x" then 3 else error "not defined") (Mult (Const 4) (Const 2))
-- Const 8
evalE ::
  (Num n) =>
  -- | The lookup table to look up the value of variables.
  (a -> n) ->
  Expr a n ->
  -- | The `Expr` to evaluate
  n
evalE f = foldE f id (+) (*)

-- | Simplifies an `Expr` removing redundant or superfluous terms.
--
-- I even added some extra simplification rules to also normalize slightly. I added this because
-- for the tests of `diffE` I wanted the `Expr` not to explode and be predictable.
--
-- === Examples
--
-- >>> simplifyE (Const 5)
-- Const 5
--
-- >>> simplifyE (Var "x")
-- Var "x"
--
-- >>> simplifyE (Plus (Const 5) (Const 1))
-- Const 6
--
-- >>> simplifyE (Plus (Var "x") (Const 0))
-- Var "x"
--
-- >>> simplifyE (Mult (Const 2) (Const 3))
-- Const 6
--
-- >>> simplifyE (Mult (Var "x") (Const 0))
-- Const 0
--
-- >>> simplifyE (Mult (Var "x") (Const 1))
-- Var "x"
--
-- >>> simplifyE (Plus (Var "x") (Var "x"))
-- Mult (Const 2) (Var "x")
--
-- >>> simplifyE (Plus (Var "x") (Plus (Var "x") (Var "x")))
-- Mult (Const 3) (Var "x")
--
-- >>> simplifyE (Plus (Var "x") (Mult (Const 2) (Var "x")))
-- Mult (Const 3) (Var "x")
simplifyE ::
  (Eq a, Eq n, Num n) =>
  -- | The `Expr` to simplify
  Expr a n ->
  -- | The simplified `Expr`
  Expr a n
simplifyE = foldE Var Const plusCase multCase
  where
    -- Step case: Multiplication
    multCase left right = case (left, right) of
      (Const l, Const r) -> Const (l * r)
      -- x * 0 = 0
      (Const 0, _) -> Const 0
      (_, Const 0) -> Const 0
      -- x * 1 = x
      (Const 1, r) -> r
      (l, Const 1) -> l
      -- (c * x) * d = (c + d) * x = e * x
      (Mult (Const c1) (Var v), Const c2) -> Mult (Const (c1 * c2)) (Var v)
      (Mult (Var v) (Const c1), Const c2) -> Mult (Const (c1 * c2)) (Var v)
      (Const c1, Mult (Const c2) (Var v)) -> Mult (Const (c1 * c2)) (Var v)
      (Const c1, Mult (Var v) (Const c2)) -> Mult (Const (c1 * c2)) (Var v)
      -- Else
      (l, r) -> Mult l r

    -- Step case: Addition
    plusCase left right = case (left, right) of
      (Const c1, Const c2) -> Const (c1 + c2)
      -- 0 + x = x + 0 = x
      (Const 0, r) -> r
      (l, Const 0) -> l
      -- x + x = 2 * x
      (Var v1, Var v2) | v1 == v2 -> Mult (Const 2) (Var v1)
      -- x + c * x = (c + 1) * x
      (Var v1, Mult (Const c) (Var v2)) | v1 == v2 -> Mult (Const (c + 1)) (Var v1)
      (Var v1, Mult (Var v2) (Const c)) | v1 == v2 -> Mult (Const (c + 1)) (Var v1)
      (Mult (Const c) (Var v1), Var v2) | v1 == v2 -> Mult (Const (c + 1)) (Var v1)
      (Mult (Var v1) (Const c), Var v2) | v1 == v2 -> Mult (Const (c + 1)) (Var v1)
      -- c * x + d * x = (c + d) * x
      (Mult (Const c1) (Var v1), Mult (Const c2) (Var v2)) | v1 == v2 -> Mult (Const (c1 + c2)) (Var v1)
      (Mult (Const c1) (Var v1), Mult (Var v2) (Const c2)) | v1 == v2 -> Mult (Const (c1 + c2)) (Var v1)
      (Mult (Var v1) (Const c1), Mult (Const c2) (Var v2)) | v1 == v2 -> Mult (Const (c1 + c2)) (Var v1)
      (Mult (Var v1) (Const c1), Mult (Var v2) (Const c2)) | v1 == v2 -> Mult (Const (c1 + c2)) (Var v1)
      -- Else
      (l, r) -> Plus l r

-- | Differentiates an `Expr` for one given `Var`.
--
-- Even cleans the result by calling `simplifyE` to simplify the resulting `Expr`. Otherwise the
-- `Expr` can explode in size.
--
-- === Examples
--
-- >>> diffE "x" (Const 5)
-- Const 0
--
-- >>> diffE "x" (Var "x")
-- Const 1
--
-- >>> diffE "x" (Var "y")
-- Const 0
--
-- >>> diffE "x" (Mult (Const 5) (Var "x"))
-- Const 5
diffE ::
  (Eq n, Num n, Eq a) =>
  -- | The `Var` to differentiate in respect to.
  a ->
  -- | The `Expr` to differentiate.
  Expr a n ->
  -- | The resulting `Expr` after differentiating.
  Expr a n
diffE variable expr = let (_, result) = diff expr in simplifyE result
  where
    diff = foldE varCase constCase plusCase multCase
    varCase v = (Var v, if v == variable then Const 1 else Const 0)
    constCase c = (Const c, Const 0)
    plusCase (f, f') (g, g') = (Plus f g, Plus f' g')
    multCase (f, f') (g, g') = (Mult f g, Plus (Mult f g') (Mult f' g))

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

-- |  The name of the program.
program :: String
program = "formulator"

-- | The help message provided
helpMsg :: String
helpMsg =
  unlines
    [ "USAGE: " ++ program ++ " <expression> [-p|-s|-d|-e|-h]",
      "",
      "  Where <expression> is an expression parsable by parseExpr.",
      "",
      "OPTIONS:",
      "",
      "  -h, --help                   show this help",
      "  -p, --print                  pretty-print expression [default]",
      "  -s, --simplify               simplify and print",
      "  -d, --differentiate <VAR>    differentiate wrt variable",
      "  -e, --evaluate <LOOKUP>      evaluate with lookup like 'x=4;y=5'",
      ""
    ]

-- | The actions the program can take.
data Action
  = -- | The `Action` to pretty print the `Expr` using `printE`.
    Print
  | -- | The `Action` to simplify the `Expr` using `simplifyE`.
    Simplify
  | -- | The `Action` to differentiate the `Expr` using `diffE` with respect to some `Var`.
    Differentiate String
  | -- | The `Action` to evaluate the `Expr` using `evalE` where are `Var`s are defined.
    Evaluate String
  | -- | The `Action` to show the `helpMsg`.
    Help
  deriving (Show)

-- | The options parsed from the CLI arguments.
data Options = Options
  { -- | The expression to parse and handle.
    optExpr :: Maybe String,
    -- | The action requested by a flag.
    optAction :: Action
  }
  deriving (Show)

-- | Parse all arguments provided to the program into an instance of `Options`.
parseArgs ::
  -- | The arguments provided to the program.
  [String] ->
  -- | The result of the parsing with:
  -- - Left: some error string.
  -- - Right: The options parsed.
  Either String Options
parseArgs = rec (Options Nothing Print)
  where
    rec opts [] = Right opts
    rec opts ("-h" : _) = Right opts {optAction = Help}
    rec opts ("--help" : _) = Right opts {optAction = Help}
    rec opts ("-p" : rest) = rec opts {optAction = Print} rest
    rec opts ("--print" : rest) = rec opts {optAction = Print} rest
    rec opts ("-s" : rest) = rec opts {optAction = Simplify} rest
    rec opts ("--simplify" : rest) = rec opts {optAction = Simplify} rest
    rec opts ("-d" : var : rest) = rec opts {optAction = Differentiate var} rest
    rec opts ("--differentiate" : var : rest) = rec opts {optAction = Differentiate var} rest
    rec _ ["-d"] = Left "--differentiate requires a variable argument"
    rec _ ["--differentiate"] = Left "--differentiate requires a variable argument"
    rec opts ("-e" : tbl : rest) = rec opts {optAction = Evaluate tbl} rest
    rec opts ("--evaluate" : tbl : rest) = rec opts {optAction = Evaluate tbl} rest
    rec _ ["-e"] = Left "--evaluate requires a lookup table argument"
    rec _ ["--evaluate"] = Left "--evaluate requires a lookup table argument"
    -- anything not recognized as a flag is treated as the expression
    rec opts (posExpr : rest) =
      if "-" `isPrefixOf` posExpr
        then Left ("unknown flag: '" ++ posExpr ++ "'")
        else rec opts {optExpr = Just posExpr} rest

-- | Parse all arguments provided to the program and execute the required function returning stdout.
processCLIArgs ::
  -- | The arguments provided to the program.
  [String] ->
  -- | The stdout of the program.
  String
processCLIArgs args =
  case parseArgs args of
    Left err -> "Error: " ++ err ++ "\n\n" ++ helpMsg
    Right opts -> case optExpr opts of
      Nothing -> "Error: no expression provided.\n\n" ++ helpMsg
      Just exprStr -> case parseExpr exprStr of
        Left err -> "Error: could not parse expression: " ++ show err
        Right expr -> case optAction opts of
          Help -> helpMsg
          Print -> printE expr
          Simplify -> printE (simplifyE expr)
          Differentiate var -> printE (diffE var expr)
          Evaluate tbl -> show (evalE (toTable tbl) expr)
  where
    toTable :: String -> (String -> Integer)
    toTable tbl = \var -> fromMaybe 0 (lookup var parsed)
      where
        parsed :: [(String, Integer)]
        parsed = map parsePair (splitOn ";" tbl)

        parsePair :: String -> (String, Integer)
        parsePair str = case splitOn "=" str of
          [k, v] -> (k, read v)
          _ -> error ("Invalid lookup entry: " ++ str)

-- Bonus exercise

-- | Normalizes a given `Expr`. 
--
-- After normalizing the `Expr` represents the same value. So for example: "x + 5" = "5 + x".
--
normalizeE :: (Eq a, Num b) => Expr a b -> Expr a b
normalizeE = foldE Var Const Plus Mult

newtype Fix f = Fx (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

-- Change this data type accordingly.

data ExprF a b c = ImplementThisDataType

type Algebra f c = f c -> c

cata :: (Functor f) => Algebra f c -> Fix f -> c
cata f = f . fmap (cata f) . unFix

toExprF :: Expr a b -> Fix (ExprF a b)
toExprF = error "Implement and document this function"

fromExprF :: Fix (ExprF a b) -> Expr a b
fromExprF = error "Implement and document this function"
