module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Control.Arrow (Arrow (first))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> Val
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "Pure" $ runEval (Pure 2) @?= 2,
      testCase "ReadOP" $ runEval (Free (ReadOp (\_ -> (Pure 2)))) @?= 2,
      testCase "StatePutOp" $ runEval (Free (StatePutOp [(ValInt 10, ValInt 10)] (Free (StateGetOp (\e -> (Pure e)))))) @?= [(ValInt 10, ValInt 10)]
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    []
