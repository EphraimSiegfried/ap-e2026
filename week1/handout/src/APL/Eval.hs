module APL.Eval
  ( eval,
    Val (..),
  )
where

import APL.AST

data Val = ValInt Integer
  deriving (Eq, Show)

getInt :: Val -> Integer
getInt (ValInt x) = x

evalBin :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Val
evalBin op x y = ValInt (op (getInt (eval x)) (getInt (eval y)))

eval :: Exp -> Val
eval (CstInt x) = ValInt x
eval (Add x y) = evalBin (+) x y
eval (Sub x y) = evalBin (-) x y
eval (Mul x y) = evalBin (*) x y
eval (Div x y) = evalBin div x y
eval (Pow x y) = evalBin (^) x y
