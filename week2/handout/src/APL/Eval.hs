module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  -- f :: (a -> b)
  -- f' :: env -> Either Error a
  -- b :: env -> Either Error a
  -- return: f b
  fmap f (EvalM f') = EvalM $ \env -> case f' env of
    Left e -> Left e
    Right x -> Right $ f x

instance Applicative EvalM where
  -- x :: a
  pure x = EvalM $ \_ -> Right x

  -- f :: f (a -> b)
  -- f :: EvalM (Env -> Either Error (a -> b))
  -- x :: f a
  -- x :: EvalM (Env -> Either Error a)
  -- return :: f b
  (<*>) (EvalM f) (EvalM x) = EvalM $ \env -> case (x env, f env) of
    (Right a, Right f') -> Right $ f' a
    (Left e, _) -> Left e
    (_, Left e) -> Left e

instance Monad EvalM where
  -- x :: m a
  -- x :: EvalM (Env -> Either Error a)
  -- f :: (a -> m b)
  -- return :: m b
  EvalM s >>= f = EvalM $ \env -> case s env of
    (Left e) -> Left e
    (Right x) ->
      let EvalM g = f x
       in g env

runEval :: EvalM a -> Either Error a
runEval (EvalM f) = f envEmpty

failure :: String -> EvalM a
failure err = EvalM (\_ -> Left err)

askEnv :: EvalM Env
askEnv = EvalM (\e -> Right e)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM g) = EvalM $ \env -> g $ f env

binOpInt :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
binOpInt f e1 e2 = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ f x' y'
    _ -> failure "Expected binary integer operation"

-- catch :: EvalM a -> EvalM a -> EvalM a
-- catch (EvalM m1) (EvalM m2) = EvalM $ case (m1, m2) of
--   (Right x, _) -> Right x
--   (Left _, Right x) -> Right x
--   (Left _, Left e) -> Left e

eval :: Exp -> EvalM Val
eval (CstInt e) = pure $ ValInt e
eval (CstBool e) = pure $ ValBool e
eval (Add e1 e2) = binOpInt (+) e1 e2
eval (Sub e1 e2) = binOpInt (-) e1 e2
eval (Mul e1 e2) = binOpInt (*) e1 e2
eval (Div e1 e2) = binOpInt div e1 e2
eval (Pow e1 e2) = binOpInt (^) e1 e2
eval (Eql e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValBool x', ValBool y') -> pure $ ValBool $ x' == y'
    (ValInt x', ValInt y') -> pure $ ValBool $ x' == y'
    _ -> failure "Right and left operand are not of same type"
eval (If c e1 e2) = do
  c' <- eval c
  case c' of
    (ValBool cond) -> if cond then eval e1 else eval e2
    _ -> failure "If condition does not evaluate to a boolean"
eval (Var n) = do
  env <- askEnv
  case envLookup n env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ n
eval (TryCatch e1 e2) = eval e1 `catch` eval e2
eval _ = undefined
