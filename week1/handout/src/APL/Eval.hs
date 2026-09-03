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

type Error = String

evalBin :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Either Error Val
evalBin op x y = case eval x of
  Left ex -> Left ex
  Right xv -> case eval y of
    Left ey -> Left ey
    Right yv -> Right (ValInt (op (getInt xv) (getInt yv)))

eval :: Exp -> Either Error Val
eval (CstInt x) = Right (ValInt x)
eval (Add x y) = evalBin (+) x y
eval (Sub x y) = evalBin (-) x y
eval (Mul x y) = evalBin (*) x y
eval (Div _ (CstInt 0)) = Left "Error: Divide by zero"
eval (Div x y) = evalBin div x y
eval (Pow x (CstInt y)) = if y > 0 then evalBin (^) x (CstInt y) else Left "Error: Negative exponent"
eval (Pow x y) = evalBin (^) x y
