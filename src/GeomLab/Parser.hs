{-# LANGUAGE FlexibleContexts, UnicodeSyntax #-}

module GeomLab.Parser where

import Prelude.Unicode

import Control.Applicative

import Control.Arrow.Unicode ((⫴))

import Control.Monad (foldM)
import Control.Monad.Unicode

import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String

import Text.Parsec hiding ((<|>), optional)
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token (GenLanguageDef(LanguageDef), GenTokenParser, makeTokenParser)
import qualified Text.Parsec.Token as P

import GeomLab.Syntax

languageDef ∷ Stream s m Char ⇒ GenLanguageDef s u m
languageDef = LanguageDef {
                  P.commentStart = "{",
                  P.commentEnd = "}",
                  P.commentLine = "--",
                  P.nestedComments = True,
                  P.identStart = letter <|> char '_',
                  P.identLetter = alphaNum <|> char '_',
                  P.opStart = oneOf $ nub $ map head reservedOpNames,
                  P.opLetter = oneOf $ nub $ concatMap tail reservedOpNames,
                  P.reservedNames = ["and", "define", "div", "else", "false", "if", "in", "lambda",
                                     "let", "mod", "not", "op", "or", "then", "true", "when", "_"],
                  P.reservedOpNames = reservedOpNames,
                  P.caseSensitive = True
              }

reservedOpNames ∷ [String]
reservedOpNames = ["and", "div", "mod", "not", "or", "=", "+", "-", "$", "*", "/", "&", "~", ":", "@", "<", "<=", "<>", ">", ">="]

parser ∷ Stream s m Char ⇒ GenTokenParser s u m
parser = makeTokenParser languageDef

identifier ∷ Stream s m Char ⇒ ParsecT s u m String
identifier = P.identifier parser

reserved ∷ Stream s m Char ⇒ String → ParsecT s u m ()
reserved = P.reserved parser

reservedOp ∷ Stream s m Char ⇒ String → ParsecT s u m ()
reservedOp = P.reservedOp parser

stringLiteral ∷ Stream s m Char ⇒ ParsecT s u m String
stringLiteral = P.stringLiteral parser

naturalOrFloat ∷ Stream s m Char ⇒ ParsecT s u m (Either Integer Double)
naturalOrFloat = P.naturalOrFloat parser

lexeme ∷ Stream s m Char ⇒  ParsecT s u m a → ParsecT s u m a
lexeme = P.lexeme parser

whiteSpace ∷ Stream s m Char ⇒ ParsecT s u m ()
whiteSpace = P.whiteSpace parser

parens ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
parens = P.parens parser

brackets ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
brackets = P.brackets parser

semiSep ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m [a]
semiSep = P.semiSep parser

commaSep ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m [a]
commaSep = P.commaSep parser

parseProgram ∷ (Stream s m Char, Eq v, IsString v) ⇒ ParsecT s u m [Paragraph v]
parseProgram = whiteSpace *> semiSep parsePara <* eof

parsePara ∷ (Stream s m Char, Eq v, IsString v) ⇒ ParsecT s u m (Paragraph v)
parsePara = Expression <$> parseExpr
        <|> (reserved "define" ≫ Definition <$> parseDefn)

parseDefn ∷ (Stream s m Char, Eq v, IsString v) ⇒ ParsecT s u m (VarDefn v)
parseDefn = try parseValueDef <|> parseFuncDef

parseValueDef ∷ (Stream s m Char, Eq v, IsString v) ⇒ ParsecT s u m (VarDefn v)
parseValueDef = VarDefn <$> parseName <*> (ValueDef <$> (reservedOp "=" ≫ parseExpr))

parseFuncDef ∷ (Stream s m Char, Eq v, IsString v) ⇒ ParsecT s u m (VarDefn v)
parseFuncDef = do
                 ((f, Clause args e w) : cs) ← sepBy1 parseClause (lexeme $ char '|')
                 VarDefn f <$> foldM (merge f $ length args) (FuncDef (Clause args e w :| [])) cs
  where merge f nargs (FuncDef (c :| cs)) (f', Clause args e w)
          | f ≢ f'              = parserFail "Inconsistent function name"
          | length args ≢ nargs = parserFail "Incorrect number of arguments"
          | otherwise            = return $ FuncDef (c :| cs ⧺ [Clause args e w])
        merge _ _ _ _ = error "Not a FuncDef"

parseClause ∷ (Stream s m Char, Eq v, IsString v) ⇒ ParsecT s u m (v, Clause v)
parseClause = (,) <$> parseName
                  <*> (Clause <$> parseFormals
                              <*> (reservedOp "=" ≫ parseExpr)
                              <*> optional (reserved "when" ≫ parseExpr))

parseExpr ∷ (Stream s m Char, Eq v, IsString v) ⇒ ParsecT s u m (Expr v)
parseExpr = (reserved "if"     ≫ If     <$> parseExpr <*> (reserved "then" ≫ parseExpr) <*> (reserved "else" ≫ parseExpr))
        <|> (reserved "let"    ≫ Let    <$> parseDefn <*> (reserved "in" ≫ parseExpr))
        <|> (reserved "lambda" ≫ Lambda <$> parseFormals <*> parseExpr)
        <|> parseTerm

parseTerm ∷ (Stream s m Char, Eq v, IsString v) ⇒ ParsecT s u m (Expr v)
parseTerm = buildExpressionParser table parsePrimary
  where table = [[monop "not", monop "-"],
                 [binop ":" AssocRight],
                 [binop "*" AssocLeft, binop "/" AssocLeft, binop "$" AssocLeft],
                 [binop "+" AssocLeft, binop "-" AssocLeft, binop "&" AssocLeft],
                 [binop "@" AssocRight],
                 [binop "=" AssocLeft, binop "<" AssocLeft, binop "<=" AssocLeft,
                  binop "<>" AssocLeft, binop ">" AssocLeft, binop ">=" AssocLeft],
                 [binop "and" AssocLeft],
                 [binop "or" AssocLeft]]
        monop op = Prefix (reservedOp op ≫ return (\x   → Apply (Variable $ fromString op) [x]))
        binop op = Infix  (reservedOp op ≫ return (\x y → Apply (Variable $ fromString op) [x, y]))

parsePrimary ∷ (Stream s m Char, Eq v, IsString v) ⇒ ParsecT s u m (Expr v)
parsePrimary = Bool              <$> parseBool
           <|> Number            <$> parseNumber
           <|> (List ∘ map Char) <$> parseString
           <|> List              <$> brackets (commaSep parseExpr)
           <|> (Variable <$> parseName ≫= parsePossibleApplication)
           <|> (parens parseExpr ≫= parsePossibleApplication)

parsePossibleApplication ∷ (Stream s m Char, Eq v, IsString v) ⇒ Expr v → ParsecT s u m (Expr v)
parsePossibleApplication fe = Apply fe <$> parens (commaSep parseExpr) <|> pure fe

parseFormals ∷ (Stream s m Char, IsString v) ⇒ ParsecT s u m [Pattern v]
parseFormals = parens $ commaSep parsePattern

parsePattern ∷ (Stream s m Char, IsString v) ⇒ ParsecT s u m (Pattern v)
parsePattern = buildExpressionParser table parsePrimaryPattern
    where table = [[Infix    (reservedOp ":" ≫ pure ConsPattern) AssocRight],
                   [Postfix $ reservedOp "+" ≫ flip PlusPattern <$> parseNumber]]

parsePrimaryPattern ∷ (Stream s m Char, IsString v) ⇒ ParsecT s u m (Pattern v)
parsePrimaryPattern = (reserved "_" ≫ pure BlankPattern)
                  <|> NumberPattern <$> parseNumber
                  <|> StringPattern <$> parseString
                  <|> NamePattern   <$> parseName
                  <|> ListPattern   <$> brackets (commaSep parsePattern)

parseName ∷ (Stream s m Char, IsString v) ⇒ ParsecT s u m v
parseName = fromString <$> identifier
        <|> (reserved "op" ≫ choice (map (\op → reservedOp op ≫ return (fromString op)) reservedOpNames))

parseBool ∷ Stream s m Char ⇒ ParsecT s u m Bool
parseBool = (reserved "true"  ≫ pure True)
        <|> (reserved "false" ≫ pure False)

parseNumber ∷ Stream s m Char ⇒ ParsecT s u m ℚ
parseNumber = (reservedOp "-" ≫ negate <$> parseNumber)
          <|> (toRational ⫴ toRational) <$> naturalOrFloat

parseString ∷ Stream s m Char ⇒ ParsecT s u m String
parseString = stringLiteral
