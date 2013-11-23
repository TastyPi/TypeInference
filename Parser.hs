module Parser where

import Prelude hiding (GT, LT)

import Control.Monad

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Number
import Text.Parsec.String
import Text.Parsec.Token hiding (identifier, reserved, reservedOp, stringLiteral, lexeme,
                                 whiteSpace,parens, brackets, semiSep, commaSep)
import qualified Text.Parsec.Token as P

import Syntax

languageDef :: LanguageDef st
languageDef = emptyDef {
                commentStart = "{*",
                commentEnd = "*}",
                identLetter = alphaNum <|> char '_',
                opStart = oneOf "admno=+-$*/&~:@<>",
                opLetter = oneOf "ndivotr",
                reservedNames = ["and", "define", "div", "else", "false", "if", "in", "lambda",
                                 "let", "mod", "not", "op", "or", "then", "true", "when", "_"],
                reservedOpNames = ["and", "div", "mod", "not", "or", "=", "+", "-", "$", "*", "/",
                                   "&", "~", ":", "@", "<", "<=", "<>", ">", ">="]
              }

lexer = makeTokenParser languageDef

TokenParser {
  P.identifier = identifier,
  P.reserved = reserved,
  P.reservedOp = reservedOp,
  P.stringLiteral = stringLiteral,
  P.lexeme = lexeme,
  P.whiteSpace = whiteSpace,
  P.parens = parens,
  P.brackets = brackets,
  P.semiSep = semiSep,
  P.commaSep = commaSep
} = lexer

uncurry3 f (a, b, c) = f a b c

parseProgram :: Parser [Paragraph]
parseProgram = do
                 whiteSpace
                 ps <- semiSep (lexeme parsePara)
                 eof
                 return ps

parsePara :: Parser Paragraph
parsePara = (liftM Expression parseExpr)
        <|> (reserved "define" >> liftM Definition parseDefn)

parseDefn :: Parser Defn
parseDefn = parseValueDef
        <|> parseFuncDef

parseValueDef :: Parser Defn
parseValueDef = do
                  x <- parseName
                  reservedOp "="
                  e <- parseExpr
                  return (ValueDef x e)

parseFuncDef :: Parser Defn
parseFuncDef = do
                 ((f, args, e, w):cs) <- sepBy1 parseClause (char '|')
                 foldM (merge (length args)) (FuncDef f [(args, e, w)]) cs
  where merge n (FuncDef f cs) (f', args, e, w) = if f == f'
                                                 then if length args == n
                                                      then return $ FuncDef f $ cs ++ [(args, e, w)]
                                                      else parserFail "Incorrect number of arguments"
                                                 else parserFail "Incorrect function name"

parseClause :: Parser (Name, [Name], Expr, Maybe Expr)
parseClause = do
                f <- parseName
                args <- parseFormals
                reservedOp "="
                e <- parseExpr
                w <- try (reserved "when" >> parseExpr >>= return . Just)
                     <|> return Nothing
                return (f, args, e, w)

parseExpr :: Parser Expr
parseExpr = parseTerm
        <|> (do
               f <- parseTerm
               args <- parens $ commaSep parseExpr
               return (Apply f args))
        <|> (do
               reserved "if"
               cond <- parseTerm
               reserved "then"
               e <- parseExpr
               reserved "else"
               e' <- parseExpr
               return (If cond e e'))
        <|> (do
               reserved "let"
               d <- parseDefn
               reserved "in"
               e <- parseExpr
               return (Let d e))
        <|> (do
               reserved "lambda"
               args <- parseFormals
               e <- parseExpr
               return (Lambda args e))

parseTerm :: Parser Expr
parseTerm = try (do
                   f <- parsePrimary
                   args <- parens $ commaSep parseExpr
                   return (Apply f args))
        <|> buildExpressionParser table parsePrimary
  where table = [[monop "not", monop "-"],
                 [binop ":" AssocRight],
                 [binop "*" AssocLeft, binop "/" AssocLeft, binop "$" AssocLeft],
                 [binop "+" AssocLeft, binop "-" AssocLeft, binop "&" AssocLeft],
                 [binop "@" AssocRight],
                 [binop "=" AssocLeft, binop "<" AssocLeft, binop "<=" AssocLeft,
                  binop "<>" AssocLeft, binop ">" AssocLeft, binop ">=" AssocLeft],
                 [binop "and" AssocLeft],
                 [binop "or" AssocLeft]]
        monop name = Prefix (reservedOp name >> return (\x -> Apply (Variable name) [x]))
        binop name assoc = Infix (reservedOp name >> return (\x y -> Apply (Variable name) [x, y])) assoc

parsePrimary :: Parser Expr
parsePrimary = liftM Bool parseBool
           <|> liftM Number parseNumber
           <|> liftM (List . map Char) parseString
           <|> liftM List (brackets $ commaSep parseExpr)
           <|> liftM Variable parseName
           <|> parens parseExpr

parseFormals :: Parser [Pattern]
parseFormals = parens $ commaSep parsePattern

-- TODO: Pattern matching
parsePattern :: Parser Pattern
parsePattern = parseName

parseName :: Parser Name
parseName = try identifier
        <|> (reserved "op" >> choice (map (\op -> reservedOp op >> return op) (reservedOpNames languageDef)))

parseBool :: Parser Bool
parseBool = (reserved "true" >> return True)
        <|> (reserved "false" >> return False)

parseNumber :: Floating f => Parser f
parseNumber = floating2 True

parseString :: Parser String
parseString = stringLiteral
