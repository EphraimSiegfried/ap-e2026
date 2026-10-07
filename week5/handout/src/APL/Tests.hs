module APL.Tests where

import APL.AST (Exp (..), VName)
import APL.Check (Error)
import APL.Eval (Val, eval, runEval)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, listOf1, oneof, shrink, sized)

eval' :: Exp -> Either Error Val
eval' s = runEval $ eval s

genVar :: Gen VName
genVar = listOf1 $ elements "abcdefghijklmnopqrstuvwxzy"

genExp :: Int -> Gen Exp
genExp size =
  if size <= 1
    then
      oneof
        [ Var <$> genVar,
          CstInt <$> (arbitrary :: Gen Integer),
          CstBool <$> (arbitrary :: Gen Bool)
        ]
    else
      let half = (size - 1) `div` 2
          third = (size - 1) `div` 3
       in oneof
            [ Var <$> genVar,
              CstInt <$> (arbitrary :: Gen Integer),
              CstBool <$> (arbitrary :: Gen Bool),
              Add <$> genExp half <*> genExp half,
              Sub <$> genExp half <*> genExp half,
              Mul <$> genExp half <*> genExp half,
              Div <$> genExp half <*> genExp half,
              Pow <$> genExp half <*> genExp half,
              Eql <$> genExp half <*> genExp half,
              TryCatch <$> genExp half <*> genExp half,
              If <$> genExp third <*> genExp third <*> genExp third,
              Lambda <$> genVar <*> genExp (size - 1),
              Apply <$> genExp half <*> genExp half,
              Let <$> genVar <*> genExp (half - 1) <*> genExp (half - 1)
            ]

prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc n1 n2 n3 = (n1 + n2) + n3 == n1 + (n2 + n3)

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc n1 n2 n3 = eval' ((n1 `Add` n2) `Add` n3) == eval' (n1 `Add` (n2 `Add` n3))

instance Arbitrary Exp where
  arbitrary = sized genExp

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Var x) =
    [Var x' | x' <- shrink x, not (null x')]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    e : [Lambda x e' | e' <- shrink e]
