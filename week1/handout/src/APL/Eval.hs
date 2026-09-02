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

eval :: Exp -> Val
eval (CstInt x) = ValInt x
eval (Add x y) = ValInt ((getInt (eval x)) + (getInt (eval y)))
eval (Sub x y) = ValInt ((getInt (eval x)) - (getInt (eval y)))
eval (Mul x y) = ValInt ((getInt (eval x)) * (getInt (eval y)))
eval (Div x y) = ValInt ((getInt (eval x)) `div` (getInt (eval y)))
eval (Pow x y) = ValInt ((getInt (eval x)) ^ (getInt (eval y)))
