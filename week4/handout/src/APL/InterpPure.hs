module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> a
runEval = runEval' envEmpty
  where
    runEval' :: Env -> EvalM a -> a
    runEval' _ (Pure x) = x
    runEval' r (Free (ReadOp k)) =
      let s = k r
       in runEval' r $ s
