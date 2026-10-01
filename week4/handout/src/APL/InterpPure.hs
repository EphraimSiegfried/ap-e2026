module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> a
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> a
    runEval' _ _ (Pure x) = x
    runEval' r st (Free (ReadOp k)) =
      let s = k r
       in runEval' r st s
    runEval' r st (Free (StateGetOp k)) =
      let c = k st
       in runEval' r st c
    runEval' r _ (Free (StatePutOp st' c)) = runEval' r st' c
