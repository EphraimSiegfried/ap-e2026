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
        eval (CstInt 3) @?= Right (ValInt 3),
      testCase "Addition" $
        eval (Add (CstInt 2) (CstInt 4)) @?= Right (ValInt 6),
      testCase "Subtraction" $
        eval (Sub (CstInt 4) (CstInt 2)) @?= Right (ValInt 2),
      testCase "Multiplication" $
        eval (Mul (CstInt 3) (CstInt 4)) @?= Right (ValInt 12),
      testCase "Power" $
        eval (Pow (CstInt 3) (CstInt 4)) @?= Right (ValInt 81),
      testCase "Division" $
        eval (Div (CstInt 8) (CstInt 3)) @?= Right (ValInt 2),
      testCase "Nested Calculation" $
        eval (Mul (Add (CstInt 3) (CstInt 1)) (Sub (CstInt 9) (CstInt 1)))
          @?= Right
            (ValInt 32),
      testCase "Divide by zero throws error" $
        eval (Div (CstInt 9) (CstInt 0)) @?= Left "Error: Divide by zero",
      testCase "Negative exponent throws error" $
        eval (Pow (CstInt 9) (CstInt (-1))) @?= Left "Error: Negative exponent"
    ]
