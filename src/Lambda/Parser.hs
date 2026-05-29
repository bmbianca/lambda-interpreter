{-# LANGUAGE OverloadedStrings #-}

module Lambda.Parser (
    parseTerm,
    pTerm
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Lambda.AST (Term(..), Id)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Parser pentru variabile
pVar :: Parser Term
pVar = Var <$> lexeme (some (alphaNumChar <|> char '_'))

-- Parser pentru abstractizări lambda (\x. y)
pLambda :: Parser Term
pLambda = do
    _ <- symbol "\\"
    idStr <- lexeme (some alphaNumChar)
    _ <- symbol "."
    term <- pTerm
    return (Lambda idStr term)

-- Parser pentru atomi (expresii în paranteze sau variabile)
pAtom :: Parser Term
pAtom = between (symbol "(") (symbol ")") pTerm <|> pVar

-- Parser principal pentru termeni (inclusiv aplicații succesive)
pTerm :: Parser Term
pTerm = pLambda <|> do
    atoms <- some pAtom
    return $ foldl1 App atoms

-- Funcție utilitară care combină totul și rulează parserul pe un string
parseTerm :: String -> Either (ParseErrorBundle String Void) Term
parseTerm = parse (sc *> pTerm <* eof) ""