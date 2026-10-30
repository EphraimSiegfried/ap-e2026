module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ -- Example tests
      parserTest "x+y" $ Add (Var "x") (Var "y"),
      parserTest "(2+2,3)" $ Tuple [Add (CstInt 2) (CstInt 2), (CstInt 3)],
      parserTest "((2,1),3)" $ Tuple [Tuple [(CstInt 2), (CstInt 1)], (CstInt 3)],
      parserTest "(2)" $ CstInt 2,
      parserTest "loop p = 10 while p == 0 do p - 1" $ WhileLoop ("p", CstInt 10) (Eql (Var "p") (CstInt 0)) (Sub (Var "p") (CstInt 1)),
      parserTest "2 || 3 " $ OneOf (CstInt 2) (CstInt 3),
      parserTest "2 && 3 " $ BothOf (CstInt 2) (CstInt 3),
      parserTest "2 + 3 && 3 " $ Add (CstInt 2) (BothOf (CstInt 3) (CstInt 3)),
      parserTestFail "x+"
    ]
