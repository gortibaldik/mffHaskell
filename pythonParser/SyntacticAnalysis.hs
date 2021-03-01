module SyntacticAnalysis where

import TokenParser
import Text.Megaparsec
import Data.Void (Void)

type Parser = Parsec Void TokStream --parsujeme seznam otagovaných tokenů
type Identifier = String
type IdArgs = [Identifier]
type ExprArgs = [Expr]

data Expr = Expr String Expr Expr
          | EInt Int 
          | EStrLit String
          | EId String
          | EFuncCall Expr ExprArgs
  deriving (Show)
data Statement = SAssign Expr Expr
               | SExpr Expr
               | SBlock [Statement]
               | SWhile Expr Statement
               | SIfElse Expr Statement (Maybe Statement)
               | SFuncDef Expr ExprArgs Statement
  deriving (Show)

data Types = TypInt | TypString | TypId | TypNL | TypDivOp | TypSumOp | TypEq | TypCmp|
             TypOpenBracket | TypCloseBracket | TypAssign | TypComma | TypBlockStart |
             TypBlockEnd | TypWhile | TypColon | TypIf | TypElse | TypDef

typeCheck :: Types -> Tkn -> Bool
typeCheck TypInt (TInt _) = True
typeCheck TypString (TString _) = True
typeCheck TypId (TId _) = True
typeCheck TypNL TNewline = True
typeCheck TypDivOp (TDivProd _) = True
typeCheck TypSumOp (TSumDif _) = True
typeCheck TypEq (TEq _) = True
typeCheck TypCmp (TCmp _) = True
typeCheck TypOpenBracket TOpenBracket = True
typeCheck TypCloseBracket TCloseBracket = True
typeCheck TypAssign TAssign = True
typeCheck TypComma TComma = True
typeCheck TypBlockStart TBlockStart = True
typeCheck TypBlockEnd TBlockEnd = True
typeCheck TypWhile TWhile = True
typeCheck TypColon TColon = True
typeCheck TypIf TIf = True
typeCheck TypElse TElse = True
typeCheck TypDef TDef = True
typeCheck _ _ = False

createPrimaryExpr :: Tkn -> Expr
createPrimaryExpr (TInt i) = EInt i
createPrimaryExpr (TString str) = EStrLit str
createPrimaryExpr (TId str) = EId str

maybeTknToStr :: Maybe Tkn -> String
maybeTknToStr (Just (TEq op)) = op
maybeTknToStr (Just (TCmp op)) = op
maybeTknToStr (Just (TDivProd op)) = [op]
maybeTknToStr (Just (TSumDif op)) = [op]

analyzeSyntax :: TokStream -> Either (ParseErrorBundle TokStream Void) [Statement]
analyzeSyntax = runParser (many pStatement <* eof) ""

pBlock :: Parser Statement
pBlock = do
  ( () <$ satisfy (typeCheck TypNL ) <?> "a newline") <|> pure ()
  satisfy (typeCheck TypBlockStart) <?> "a block of commands"
  statements <- many pStatement <* satisfy (typeCheck TypBlockEnd)
  return $ SBlock statements

pAssign :: Parser Statement
pAssign = (SAssign <$> fst <*> snd ) <$> ( (,) <$> (( createPrimaryExpr <$> satisfy (typeCheck TypId) <?> "an identifier" ) <* satisfy (typeCheck TypAssign) <?> "an equal sign") <*> pExpr)

pStatementSimple :: Parser Statement
pStatementSimple = choice [ try pAssign, SExpr <$> pExpr ]

pStatement :: Parser Statement
pStatement = do
  many $ satisfy (typeCheck TypNL)
  res <- choice [pBlock, pStatementSimple, pWhile, pIfElse, pFuncDef]
  many (satisfy (typeCheck TypNL)) <|> pure ([] :: [Tkn])
  return res

pWhile :: Parser Statement
pWhile = do
  satisfy (typeCheck TypWhile) <?> "while statement"
  expr <- (pExpr <?> "a condition") <* (satisfy (typeCheck TypColon) <?> ":")
  SWhile expr <$> pStatement

pIfElse :: Parser Statement
pIfElse = do
  satisfy (typeCheck TypIf) <?> "if statement"
  expr <- (pExpr <?> "a condition") <* (satisfy (typeCheck TypColon) <?> ":")
  ifStatement <- pStatement
  SIfElse expr ifStatement <$> optional pElse
  where pElse = do
          satisfy (typeCheck TypElse) <?> "else statement"
          satisfy (typeCheck TypColon) <?> ":"
          pStatement

pFuncDef :: Parser Statement
pFuncDef = do
  satisfy (typeCheck TypDef) <?> "def keyword"
  name <- createPrimaryExpr <$> satisfy (typeCheck TypId) <?> "a function name"
  args <- pHelperParentheses (pExprs (createPrimaryExpr <$> satisfy (typeCheck TypId) <?> "an identifier"))
  satisfy (typeCheck TypColon) <?> ":"
  SFuncDef name args <$> pStatement


pExprs :: Parser Expr -> Parser [Expr]
pExprs parser = try ( (:) <$> parser <*> many (satisfy (typeCheck TypComma) *> parser) ) <|> pure ([] :: [Expr])

pExpr, pEq, pCmp, pTerm, pFactor :: Parser Expr
pExpr = pEq
pEq = pHelper pCmp TypEq
pCmp = pHelper pTerm TypCmp
pTerm = pHelper pFactor TypSumOp
pFactor = pHelper pPrimary TypDivOp

pPrimary :: Parser Expr
pPrimary = choice [
  createPrimaryExpr <$> satisfy (typeCheck TypInt) <?> "an integer",
  createPrimaryExpr <$> satisfy (typeCheck TypString) <?> "a string literal",
  try $ (EFuncCall <$> fst <*> snd ) <$> ( (,) <$> (createPrimaryExpr <$> satisfy (typeCheck TypId) <?> "a function name") <*> pHelperParentheses (pExprs pExpr)),
  createPrimaryExpr <$> satisfy (typeCheck TypId) <?> "an identifier",
  pHelperParentheses pExpr
  ]

pHelper :: Parser Expr -> Types -> Parser Expr
pHelper pSub tp = do
  term <- pSub
  maybe <- optional $ satisfy $ typeCheck tp
  case maybe of
    Nothing -> return term
    just -> Expr (maybeTknToStr just) term <$> pHelper pSub tp

pHelperParentheses :: Parser a -> Parser a
pHelperParentheses parser = (satisfy (typeCheck TypOpenBracket) <?> "(") *> parser <* (satisfy (typeCheck TypCloseBracket) <?> ")")