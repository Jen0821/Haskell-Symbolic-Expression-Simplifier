module Simplify where
-- Write all of your code in this file!

import Expr
import Poly

--------------------------------------------------------------------------------
-- * Task 1
-- Define add, which adds 2 expressions together without introducing
-- any 'junk'.

add :: Expr -> Expr -> Expr
add (NumLit 0) expr = expr
add expr (NumLit 0) = expr
add (NumLit n1) (NumLit n2) = NumLit (n1 + n2)
add (ExpX n1) (ExpX n2)
  | n1 == n2  = Op MulOp (NumLit 2) (ExpX n1)
add expr1 expr2 = Op AddOp expr1 expr2


--------------------------------------------------------------------------------
-- * Task 2
-- Define mul, which multiplies 2 expressions together without introducing
-- any 'junk'.

mul :: Expr -> Expr -> Expr
mul (NumLit 0) _ = NumLit 0
mul _ (NumLit 0) = NumLit 0
mul (NumLit 1) expr = expr
mul expr (NumLit 1) = expr
mul (NumLit n1) (NumLit n2) = NumLit (n1 * n2)
mul (ExpX n1) (ExpX n2) = ExpX (n1 + n2)
mul expr1 expr2 = Op MulOp expr1 expr2

--------------------------------------------------------------------------------
-- * Task 3
-- Define addAll, which adds a list of expressions together into
-- a single expression without introducing any 'junk'.

addAll :: [Expr] -> Expr
addAll [] = NumLit 0
addAll [expr] = expr
addAll (expr:exprs) = add expr (addAll exprs)

--------------------------------------------------------------------------------
-- * Task 4
-- Define mulAll, which multiplies a list of expressions together into
-- a single expression without introducing any 'junk'.

mulAll :: [Expr] -> Expr
mulAll [] = NumLit 1
mulAll [expr] = expr
mulAll (expr:exprs) = mul expr (mulAll exprs)

--------------------------------------------------------------------------------
-- * Task 5
-- Define exprToPoly, which converts an expression into a polynomial.

exprToPoly :: Expr -> Poly
exprToPoly (Op MulOp (NumLit x)(ExpX y)) = listToPoly(map(\z -> if z == y then x else 0)[y,(y-1)..0])
exprToPoly (NumLit x)                    = listToPoly [x]
exprToPoly (ExpX x)                      = listToPoly(map(\z -> if x == z then 1 else 0)[x,(x-1)..0])
exprToPoly (Op MulOp x y)                = mulPoly (exprToPoly x) (exprToPoly y)
exprToPoly (Op AddOp x y)                = addPoly (exprToPoly x) (exprToPoly y)

--------------------------------------------------------------------------------
-- * Task 6
-- Define polyToExpr, which converts a polynomial into an expression.

polyToExpr :: Poly -> Expr
polyToExpr e
    | null x                     = NumLit 0
    | length (polyToList e) == 1 = NumLit (head (polyToList e))
    | otherwise                  = add (mul (NumLit (head x))(ExpX (length x -1))) (polyToExpr (listToPoly (drop 1 x)))
        where x = polyToList e

--------------------------------------------------------------------------------
-- * Task 7
-- Define a function which simplifies an expression by converting it to a
-- polynomial and back again.

simplify :: Expr -> Expr
simplify expr = polyToExpr (exprToPoly expr)

--------------------------------------------------------------------------------