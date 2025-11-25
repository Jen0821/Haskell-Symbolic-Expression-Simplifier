module Expr
  ( Expr(..)
  , BinOp(..)
  , example1, example2, example3, example4, example5
  , eval
  , prettyExpr
  , printExpr
  ) where

data Expr
  = Op BinOp Expr Expr
  | NumLit Int
  | ExpX Int
  deriving (Eq, Show)

data BinOp = AddOp | MulOp
  deriving (Eq, Show)

example1, example2, example3, example4, example5 :: Expr
example1 = Op AddOp (NumLit 1) (Op AddOp (NumLit 2) (NumLit 3))
example2 = Op AddOp (NumLit 1) (Op MulOp (NumLit 2) (NumLit 3))
example3 = Op MulOp (NumLit 1) (Op AddOp (NumLit 2) (NumLit 3))
example4 = ExpX 4
example5 = Op MulOp (NumLit 1) (Op AddOp (ExpX 4) (NumLit 3))

eval :: Int -> Expr -> Int
eval x (ExpX n) = x^n
eval _ (NumLit y) = y
eval x (Op AddOp a b) = eval x a + eval x b
eval x (Op MulOp a b) = eval x a * eval x b

printExpr :: Expr -> IO ()
printExpr expr = putStrLn (prettyExpr expr)

prettyExpr :: Expr -> String
prettyExpr (NumLit x) = show x
prettyExpr (ExpX 1) = "x"
prettyExpr (ExpX n) = "x^" ++ show n
prettyExpr (Op AddOp x y) = prettyExpr x ++ " + " ++ prettyExpr y
prettyExpr (Op MulOp x@(Op AddOp x1 x2) y@(Op AddOp y1 y2))
  = concat ["(", prettyExpr x, ") * (", prettyExpr y, ")"]
prettyExpr (Op MulOp x@(Op AddOp x1 x2) y)
  = concat ["(", prettyExpr x, ") * ", prettyExpr y]
prettyExpr (Op MulOp x y@(Op AddOp y1 y2))
  = concat [prettyExpr x, " * (", prettyExpr y, ")"]
prettyExpr (Op MulOp x y)
  = concat [prettyExpr x, " * ", prettyExpr y]
