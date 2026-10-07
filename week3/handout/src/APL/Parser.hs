module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pExp1 :: Parser Exp
pExp1 = pAtom >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pAtom
            chain $ Mul x y,
          do
            lString "/"
            y <- pAtom
            chain $ Div x y,
          pure x
        ]

pLExp :: Parser Exp
pLExp = do
  lKeyword "if"
  cond <- pExp0
  lKeyword "then"
  e1 <- pExp0
  lKeyword "else"
  e2 <- pExp0
  pure $ If cond e1 e2

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp :: Parser Exp
pExp =
  choice
    [ pExp0,
      pLExp
    ]

keywords :: [String]
keywords = ["true", "false", "if", "then", "else"]

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lexeme :: Parser a -> Parser a
lexeme g = g <* space

lInteger :: Parser Integer
lInteger = do
  x <- lexeme $ some $ satisfy isDigit
  notFollowedBy $ satisfy isAlpha
  pure $ read x

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ do
  void $ chunk s
  notFollowedBy $ satisfy isAlpha

pBool :: Parser Bool
pBool =
  choice
    [ lKeyword "true" >> pure True,
      lKeyword "false" >> pure False
    ]

lVName :: Parser VName
lVName = try $ lexeme $ do
  x <- some $ satisfy isAlpha
  y <- many $ satisfy isAlphaNum
  let v = x ++ y
   in if (v `elem` keywords) then fail $ "Unexpected keyword" else pure v
