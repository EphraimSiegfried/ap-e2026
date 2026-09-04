module APL.Eval
  ( eval,
    Val (..),
    envEmpty,
  )
where

import APL.AST

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend name v env = [(name, v)] ++ env

envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

evalBinInt :: Env -> (Integer -> Integer -> Integer) -> Exp -> Exp -> Either Error Val
evalBinInt env op x y = case (eval env x, eval env y) of
  (Left ex, _) -> Left ex
  (_, Left ey) -> Left ey
  (Right (ValInt xi), Right (ValInt yi)) -> Right (ValInt (op xi yi))
  _ -> Left "Error: Left and right operand aren't the same type"

evalEql :: Env -> Exp -> Exp -> Either Error Val
evalEql env x y = case (eval env x, eval env y) of
  (Left ex, _) -> Left ex
  (_, Left ey) -> Left ey
  (Right (ValBool xi), Right (ValBool yi)) -> Right (ValBool (xi == yi))
  (Right (ValInt xb), Right (ValInt yb)) -> Right (ValBool (xb == yb))
  _ -> Left "Error: Left and right operand aren't the same type"

evalIf :: Env -> Exp -> Exp -> Exp -> Either Error Val
evalIf env cond thenb elseb = case (eval env cond) of
  (Right (ValBool True)) -> eval env thenb
  (Right (ValBool False)) -> eval env elseb
  (Left e) -> Left e
  _ -> Left "Error: Couldn't evaluate if condition"

eval :: Env -> Exp -> Either Error Val
eval _ (CstInt x) = Right (ValInt x)
eval _ (CstBool x) = Right (ValBool x)
eval env (Eql x y) = evalEql env x y
eval env (If x y z) = evalIf env x y z
eval env (Add x y) = evalBinInt env (+) x y
eval env (Sub x y) = evalBinInt env (-) x y
eval env (Mul x y) = evalBinInt env (*) x y
eval env (Div x y) = if eval env y == Right (ValInt 0) then Left "Error: Divide by zero" else evalBinInt env div x y
eval env (Pow x (CstInt y)) = if y >= 0 then evalBinInt env (^) x (CstInt y) else Left "Error: Negative exponent"
eval env (Pow x y) = evalBinInt env (^) x y
eval env (Var name) = case (envLookup name env) of
  Just x -> Right x
  Nothing -> Left "Error: Variable undefined"
eval env (Let name x y) = case (eval env x) of
  Left e -> Left e
  Right v ->
    let newEnv = envExtend name v env
     in eval newEnv y
