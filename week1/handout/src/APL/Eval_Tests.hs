module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Exp evals to Val" $
        eval (CstInt 3) @?= ValInt 3,
      testCase "Addition" $
        eval (Add (CstInt 2) (CstInt 4)) @?= ValInt 6,
      testCase "Subtraction" $
        eval (Sub (CstInt 4) (CstInt 2)) @?= ValInt 2,
      testCase "Multiplication" $
        eval (Mul (CstInt 3) (CstInt 4)) @?= ValInt 12,
      testCase "Power" $
        eval (Pow (CstInt 3) (CstInt 4)) @?= ValInt 81,
      testCase "Division" $
        eval (Div (CstInt 8) (CstInt 3)) @?= ValInt 2,
      testCase "Nested Calculation" $
        eval (Mul (Add (CstInt 3) (CstInt 1)) (Sub (CstInt 9) (CstInt 1))) @?= ValInt 32
    ]
