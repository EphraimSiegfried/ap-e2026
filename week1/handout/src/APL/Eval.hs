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

evalBinInt :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Either Error Val
evalBinInt op x y = case (eval x, eval y) of
  (Left ex, _) -> Left ex
  (_, Left ey) -> Left ey
  (Right (ValInt xi), Right (ValInt yi)) -> Right (ValInt (op xi yi))
  _ -> Left "Error: Left and right operand aren't the same type"

evalEql :: Exp -> Exp -> Either Error Val
evalEql x y = case (eval x, eval y) of
  (Left ex, _) -> Left ex
  (_, Left ey) -> Left ey
  (Right (ValBool xi), Right (ValBool yi)) -> Right (ValBool (xi == yi))
  (Right (ValInt xb), Right (ValInt yb)) -> Right (ValBool (xb == yb))
  _ -> Left "Error: Left and right operand aren't the same type"

evalIf :: Exp -> Exp -> Exp -> Either Error Val
evalIf cond thenb elseb = case (eval cond) of
  (Right (ValBool True)) -> eval thenb
  (Right (ValBool False)) -> eval elseb
  (Left e) -> Left e
  _ -> Left "Error: Couldn't evaluate if condition"

eval :: Exp -> Either Error Val
eval (CstInt x) = Right (ValInt x)
eval (CstBool x) = Right (ValBool x)
eval (Eql x y) = evalEql x y
eval (If x y z) = evalIf x y z
eval (Add x y) = evalBinInt (+) x y
eval (Sub x y) = evalBinInt (-) x y
eval (Mul x y) = evalBinInt (*) x y
eval (Div x y) = if eval y == Right (ValInt 0) then Left "Error: Divide by zero" else evalBinInt div x y
eval (Pow x (CstInt y)) = if y >= 0 then evalBinInt (^) x (CstInt y) else Left "Error: Negative exponent"
eval (Pow x y) = evalBinInt (^) x y
