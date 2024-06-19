module Lexing(Parser, Term(..), Ground, Typing,rev , pVar, pAs, pLitBool, pLitInt, pAbs, pApp, pTerm) where

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (letterChar, alphaNumChar, space1, upperChar, digitChar, spaceChar, space, newline)

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (liftM3)

type Parser = Parsec Void String
type Typing = String

data Term =
      TmAbs   String Typing Term
    | TmVar   String
    | TmApp   Term Term
    | TmLit   Ground
    | TmAs    Term Typing

    | TmIfElse  Term Term Term
    deriving Show

data Ground = GBool Bool | GInt Int | GUnit deriving Show


sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

scope :: Char -> Char -> Parser a -> Parser a
scope l r p = 
       symbol [l]
    *> optional newline
    *> p 
    <* optional newline 
    <* symbol [r]


lexeme :: Parser String -> Parser String
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

var :: Parser String
var = lexeme $ (:) <$> letterChar <*> many alphaNumChar

sign :: Parser String
sign = symbol ":" *> lexeme ((:) <$> upperChar <*> many alphaNumChar)

rev :: Parser a -> Parser a
rev p = do
    o  <- getOffset
    s' <- endWith p
    r  <- p
    setInput s'
    setOffset o
    return r

endWith :: Parser a -> Parser String
endWith p =
        try (do {_ <- lookAhead (p *> many spaceChar *> eof); return []})
    <|> (do {x <- anySingle; xs <- endWith p; return (x:xs)})

-- Implementions for token parsing

pVar :: Parser Term
pVar = TmVar <$> var

pLitBool :: Parser Term
pLitBool = TmLit . GBool <$> (
        (True <$ symbol "true")
    <|> (False <$ symbol "false"))

pLitInt :: Parser Term
pLitInt = TmLit . GInt . read <$> some digitChar

pAbs :: Parser Term
pAbs = liftM3 TmAbs
        (symbol "λ" *> var)
        sign
        (symbol "." *> many newline *> pTerm 0)

pAs :: Parser Term
pAs = TmAs
    <$> (pTerm 3 <|> pParen)
    <*> sign

pApp :: Parser Term
pApp = foldr1 TmApp <$> many ((pTerm 2 <|> pParen) <* space)

pIfElse :: Parser Term
pIfElse = TmIfElse
    <$> (symbol "if"   *> (pTerm 2 <|> pParen) <* space)
    <*> (symbol "then" *> (pTerm 2 <|> pBrace) <* space)
    <*> (symbol "else" *> (pTerm 2 <|> pBrace) <* space)
    

pParen :: Parser Term
pParen = scope '(' ')' . pTerm $ 0

pBrace :: Parser Term
pBrace = scope '{' '}' . pTerm $ 0

pTerm :: Int -> Parser Term
pTerm i = choice . drop i $ 
    (++)
    [
        pIfElse
        -- Operators
      , try pApp
      , try pAs
    ]
    [
        -- Atomics
        pAbs
      , pLitBool
      , pLitInt
      , pVar
    ]