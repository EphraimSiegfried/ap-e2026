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

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "StatePutOp" $ runEval (Free (StatePutOp [(ValInt 10, ValInt 10)] (Free (StateGetOp (\e -> (Pure e)))))) @?= ([], Right [(ValInt 10, ValInt 10)]),
      testCase "Let" $ eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x")) @?= ([], Right $ ValInt 5),
      testCase "localEnv" $ runEval (localEnv (const [("x", ValInt 1)]) $ askEnv) @?= ([], Right [("x", ValInt 1)]),
      testCase "Print" $ runEval (evalPrint "hi") @?= (["hi"], Right ()),
      testCase "Error" $ eval' (Div (CstInt 1) (CstInt 0)) @?= ([], Left "Division by zero"),
      testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ())
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    []
