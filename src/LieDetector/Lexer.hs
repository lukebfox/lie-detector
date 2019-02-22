module LieDetector.Lexer (
  Operator(..),
  Token(..),
  tokenise,
  lookAhead,
  ) where

import Data.Char

data Operator = And | Or | LImply | RImply
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokNot
           | TokAssign
           | TokLParen
           | TokRParen
           | TokIdent String
           | TokBool Bool
           | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '^' = And
           | c == '|' = Or
           | c == '←' = LImply
           | c == '→' = RImply

boolean :: Char -> Bool
boolean c | c == '0' = False
          | c == '1' = True

tokenise :: String -> [Token]
tokenise [] = []
tokenise (c:cs)
    | elem c "^|←→" = TokOp (operator c) : tokenise cs
    | c == '¬' = TokNot : tokenise cs
    | c == '=' = TokAssign : tokenise cs
    | c == '(' = TokLParen : tokenise cs
    | c == ')' = TokRParen : tokenise cs
    | c == '0' = TokBool False : tokenise cs
    | c == '1' = TokBool True  : tokenise cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenise cs
    | otherwise = error $ "Cannot tokenise. " ++ [c]

identifier :: Char -> String -> [Token]
identifier c cs = let (name, cs') = span isAlphaNum cs in
                  TokIdent (c:name) : tokenise cs'

--like head but safe for empty
lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t
