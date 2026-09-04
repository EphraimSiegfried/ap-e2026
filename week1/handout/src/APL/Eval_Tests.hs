module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Exp evals to Val" $
        eval envEmpty (CstInt 3) @?= Right (ValInt 3),
      testCase "Addition" $
        eval envEmpty (Add (CstInt 2) (CstInt 4)) @?= Right (ValInt 6),
      testCase "Subtraction" $
        eval envEmpty (Sub (CstInt 4) (CstInt 2)) @?= Right (ValInt 2),
      testCase "Multiplication" $
        eval envEmpty (Mul (CstInt 3) (CstInt 4)) @?= Right (ValInt 12),
      testCase "Power" $
        eval envEmpty (Pow (CstInt 3) (CstInt 4)) @?= Right (ValInt 81),
      testCase "Division" $
        eval envEmpty (Div (CstInt 8) (CstInt 3)) @?= Right (ValInt 2),
      testCase "Nested Calculation" $
        eval envEmpty (Mul (Add (CstInt 3) (CstInt 1)) (Sub (CstInt 9) (CstInt 1)))
          @?= Right
            (ValInt 32),
      testCase "Divide by zero throws error" $
        eval envEmpty (Div (CstInt 9) (CstInt 0)) @?= Left "Error: Divide by zero",
      testCase "Negative exponent throws error" $
        eval envEmpty (Pow (CstInt 9) (CstInt (-1))) @?= Left "Error: Negative exponent",
      testCase "Equality Ints" $
        eval envEmpty (Eql (CstInt 9) (CstInt 9)) @?= Right (ValBool True),
      testCase "Equality Bools" $
        eval envEmpty (Eql (CstBool False) (CstBool False)) @?= Right (ValBool True),
      testCase "Equality with different types throws error" $
        eval envEmpty (Eql (CstBool False) (CstInt 2)) @?= Left "Error: Left and right operand aren't the same type",
      testCase "If then branch" $
        eval envEmpty (If (Eql (CstInt 0) (CstInt 0)) (CstBool True) (CstInt 60)) @?= Right (ValBool True),
      testCase "If else branch" $
        eval envEmpty (If (Eql (CstInt 2) (CstInt 1)) (CstInt 50) (CstInt 60)) @?= Right (ValInt 60),
      testCase "If condition evaluating to integer throws error" $
        eval envEmpty (If (Add (CstInt 2) (CstInt 1)) (CstInt 50) (CstInt 60)) @?= Left "Error: Couldn't evaluate if condition",
      testCase "Let" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6),
      testCase "Let fails if variable undefined" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "y") (Var "x"))) @?= Left "Error: Variable undefined"
    ]
