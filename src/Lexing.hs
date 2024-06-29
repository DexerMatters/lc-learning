module Lexing(
      Parser
    , Term(..)
    , TyTerm(..)
    , PtTerm(..)
    , FITerm
    , FI
    , Ground(..)
    , Typing
    , rev
    , pVar
    , pAs
    , pLitBool
    , pLitInt
    , pAbs
    , pApp
    , pTerm
    , lex
    ) where

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (letterChar, alphaNumChar, space1, upperChar, digitChar, spaceChar, space, newline, char)

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (liftM3, liftM4)
import Prelude hiding (lex)
import Utils.EvalEnv (Ty)

type Parser = Parsec Void String
type Typing = String
type FI = (Int, Int)


data Term =
      TmAbs     PtTerm TyTerm FITerm        -- :: λPattern Type Term
    | TmVar     String                      -- :: VariableExpression
    | TmApp     FITerm FITerm               -- :: Term Term
    | TmLit     Ground                      -- :: LiteralExpression
    | TmAs      FITerm TyTerm               -- :: Term : Type
    | TmIfElse  FITerm FITerm FITerm        -- :: if Term then Term else Term
    | TmLetIn   PtTerm TyTerm FITerm FITerm -- :: let λPattern : Type = Term in Term
    | TmProd    FITerm FITerm               -- :: (Term, Term)
    | TmProj    FITerm Int                  -- :: Term.Index

    -- for contexting
    | TmAbsC    PtTerm Ty FITerm
    | TmAsC     FITerm Ty

    -- for type contexts
    | TmTyDef   String
    | TmSubDef  String String

    deriving Show

data TyTerm =
      TmAbsT  TyTerm TyTerm
    | TmProdT TyTerm TyTerm
    | TmSiT   String
    deriving Show

data PtTerm =
      TmProdP PtTerm PtTerm
    | TmSiP   String
    deriving Show

type FITerm = (FI, Term)

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
var = lexeme $ (:) 
    <$> (letterChar <|> char '_') 
    <*> many (alphaNumChar <|> char '_')

ty :: Parser String
ty = lexeme $ (:) <$> upperChar <*> many alphaNumChar

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

fi :: Parser Term -> Parser FITerm
fi p = do
    s <- getOffset
    t <- p
    e <- getOffset
    return ((s, e), t)

unfi :: Parser FITerm -> Parser Term
unfi p = p >>= \(_, t) -> return t

-- Implementions for type expression parsing

ptParen :: Parser TyTerm
ptParen = scope '(' ')' . ptSig $ 0

ptAbs :: Parser TyTerm
ptAbs = foldl1 TmAbsT <$> sepBy1 (ptSig 1 <|> ptParen) (symbol "->")

ptProd :: Parser TyTerm
ptProd = scope '(' ')' $ foldr1 TmProdT <$> sepBy1 (ptSig 0) (symbol ",")

ptSi :: Parser TyTerm
ptSi = TmSiT <$> ty

ptSig :: Int -> Parser TyTerm
ptSig i = choice . drop i $ [try ptAbs, ptProd, ptSi]

-- Implementions for pattern expression parsing

ppProd :: Parser PtTerm
ppProd = scope '(' ')' $ foldr1 TmProdP <$> sepBy1 (ppSig 0) (symbol ",")

ppSi :: Parser PtTerm
ppSi = TmSiP <$> var

ppSig :: Int -> Parser PtTerm
ppSig i = choice . drop i $ [ppProd, ppSi]

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
        (symbol "λ" *> ppSig 0)
        (symbol ":" *> ptSig 0)
        (symbol "." *> many newline *> pTerm 0)

pAs :: Parser Term
pAs = TmAs
    <$> (pTerm 3 <|> pParen)
    <*> (symbol ":" *> ptSig 0)

pApp :: Parser Term
pApp = unfi $ foldl1 fiApp <$> many ((pTerm 2 <|> pParen) <* space) where
    fiApp ft1 ft2 = ((0, 0), TmApp ft1 ft2)

pIfElse :: Parser Term
pIfElse = liftM3 TmIfElse
        (symbol "if"   *> (pTerm 2 <|> pParen) <* space)
        (symbol "then" *> (pTerm 2 <|> pBrace) <* space)
        (symbol "else" *> (pTerm 2 <|> pBrace) <* space)

pLetIn :: Parser Term
pLetIn = liftM4 TmLetIn
        (symbol "let"   *> ppSig 0 <* space)
        (symbol ":" *> ptSig 0)
        (symbol "="     *> (pTerm 2 <|> pParen) <* space)
        (symbol "in"    *> pTerm (-1))

pTyDef :: Parser Term
pTyDef = TmTyDef <$> (symbol "::" *> ty)

pSubDef :: Parser Term
pSubDef = TmSubDef
    <$> (symbol "::" *> ty <* symbol "<:")
    <*> ty

pProd :: Parser Term
pProd = unfi $ foldr1 fiProd <$> scope '(' ')' (sepBy1 (pTerm 0) (symbol ",")) where
    fiProd ft1 ft2 = ((0, 0), TmProd ft1 ft2)

pProj :: Parser Term
pProj = TmProj
    <$> (pTerm 4 <|> pParen)
    <*> scope '[' ']' (read <$> some digitChar)

pParen :: Parser FITerm
pParen = scope '(' ')' . pTerm $ 0

pBrace :: Parser FITerm
pBrace = scope '{' '}' . pTerm $ 0

pTerm :: Int -> Parser FITerm
pTerm i = choice . drop (i + 3) $
    fi <$> [
        try pSubDef
      , pTyDef
      , pLetIn

      , pIfElse

        -- Operators
      , try pApp
      , try pAs
      , try pProj

        -- Atomics
      , pProd
      , pAbs
      , pLitBool
      , pLitInt
      , pVar
    ]

lex :: Parser [FITerm]
lex =
        do {_ <- eof; return []}
    <|> do {p <- pTerm (-3); ps <- lex; return (p:ps)}
