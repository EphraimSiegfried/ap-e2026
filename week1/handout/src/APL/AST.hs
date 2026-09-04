module APL.AST
  ( Exp (..),
  )
where

data Exp
  = CstInt Integer
  | CstBool Bool
  | Eql Exp Exp
  | If Exp Exp Exp
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  deriving (Eq, Show)
