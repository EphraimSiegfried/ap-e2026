module APL.Eval
  ( eval,
    Val (..),
  )
where

import APL.AST

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

evalBin :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Either Error Val
evalBin op x y = case (eval x, eval y) of
  (Left ex, _) -> Left ex
  (_, Left ey) -> Left ey
  (Right (ValInt xi), Right (ValInt yi)) -> Right (ValInt (op xi yi))
  (Right (ValBool xi), Right (ValBool yi)) -> Right (ValBool (op xi yi))
  _ -> Left "Error: Left and right operand aren't the same type"

eval :: Exp -> Either Error Val
eval (CstInt x) = Right (ValInt x)
eval (CstBool x) = Right (ValBool x)
eval (Eql x y) = evalBin (==) x y
eval (Add x y) = evalBin (+) x y
eval (Sub x y) = evalBin (-) x y
eval (Mul x y) = evalBin (*) x y
eval (Div _ (CstInt 0)) = Left "Error: Divide by zero"
eval (Div x y) = evalBin div x y
eval (Pow x (CstInt y)) = if y > 0 then evalBin (^) x (CstInt y) else Left "Error: Negative exponent"
eval (Pow x y) = evalBin (^) x y
