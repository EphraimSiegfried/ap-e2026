module APL.InterpSim (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

-- | Continue execution of the provided computation as far as
-- possible, but executing at most one 'StepOp' effect. Any nested
-- computations (in 'BothOp' and 'OneOfOp') must also be stepped
-- similarly. If the computation is stuck on a 'KvGetOp' for which the
-- key is not in the state, then the computation is merely returned
-- unchanged.
--
-- Evaluation of 'BothOp':
--
-- * If either of the nested computations are 'Free (ErrorOp ...)',
--   then propagate that error.
--
-- * If both are 'Pure', then return a pair of the results.
--
-- * Otherwise evaluate both one step.
--
-- Evaluation of 'OneOfOp':
--
-- * If both of the nested computations are 'Free (ErrorOp ...)', then
--   propagate one of the errors.
--
-- * If one is 'Pure', then return that result.
--
-- * Otherwise evaluate both one step.
step :: Env -> State -> EvalM a -> (EvalM a, State)
step e s (Free (StepOp c)) = (c, s)
step e s (Free (BothOfOp e1 e2 c)) = case (e1, e2) of
  (Free (ErrorOp e), _) -> (Free (ErrorOp e), s)
  (_, Free (ErrorOp e)) -> (Free (ErrorOp e), s)
  (Pure x, Pure y) -> step e s (c (ValTuple [x, y]))
  (x, y) ->
    let (x', _) = step e s x
        (y', s'') = step e s y
     in (Free (BothOfOp x' y' c), s'')
step e s (Free (OneOfOp e1 e2 c)) = case (e1, e2) of
  (Free (ErrorOp e), Free (ErrorOp _)) -> (Free (ErrorOp e), s)
  (Pure x, _) -> step e s (c x)
  (_, Pure y) -> step e s (c y)
  (x, y) ->
    let (x', _) = step e s x
        (y', s'') = step e s y
     in (Free (OneOfOp x' y' c), s'')
step e s _ = undefined

runEval :: EvalM a -> Either Error a
runEval = undefined
