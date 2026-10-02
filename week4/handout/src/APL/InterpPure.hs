module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], Right x)
    runEval' r st (Free (ReadOp k)) =
      let s = k r
       in runEval' r st s
    runEval' r st (Free (StateGetOp k)) =
      let c = k st
       in runEval' r st c
    runEval' r _ (Free (StatePutOp st' c)) = runEval' r st' c
    runEval' env state (Free (PrintOp s c)) =
      let (out, val) = runEval' env state c
       in (s : out, val)
    runEval' _ _ (Free (ErrorOp s)) = ([], Left s)
