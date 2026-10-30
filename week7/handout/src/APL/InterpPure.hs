module APL.InterpPure (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

runEval :: EvalM a -> Either Error a
runEval = fmap fst $ runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> (Either Error a, State)
    runEval' _ s (Pure x) = (pure x, s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> (Left $ "Invalid key: " ++ show key, s)
        Just val -> runEval' r s $ k val
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s
       in runEval' r s' m
    runEval' _ s (Free (ErrorOp e)) = (Left e, s)
    runEval' r s (Free (StepOp k)) = runEval' r s k
    runEval' r s (Free (OneOfOp e1 e2 k)) =
      case (runEval' r s e1, runEval' r s e2) of
        ((Right x, _), _) -> runEval' r s (k x)
        ((Left _, _), (Right x, _)) -> runEval' r s (k x)
        ((Left _, _), (Left err, s')) -> ((Left err), s)
    runEval' r s (Free (BothOfOp e1 e2 k)) =
      case (runEval' r s e1, runEval' r s e2) of
        ((Right x1, _), (Right x2, _)) -> runEval' r s (k (ValTuple [x1, x2]))
        ((Left err, s'), _) -> ((Left err), s')
        (_, (Left err, s')) -> ((Left err), s')
