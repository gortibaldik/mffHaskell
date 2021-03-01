module SemanticAnalysis where

import SyntacticAnalysis
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Control.Monad
import Data.Maybe
import PrettyPrinter

type Defined = [String]
data Specifier = Var | Func deriving (Eq)
type ErrorLog = [String]

type Traverser = ReaderT Defined (Writer ErrorLog)

parseExpr :: Expr -> String
parseExpr = ppshow

addVar :: Specifier -> Expr -> Traverser Defined
addVar spec (EId i) = do
    defined <- ask
    return $ if i `elem` defined then defined else i:defined

addLogConditional :: Bool -> String -> Traverser Bool
addLogConditional True _ = return True
addLogConditional False str = do
    (lift . tell) [str]
    return False

checkDefined :: Specifier -> Expr -> Traverser Bool
checkDefined spec (EId identifier) = do
                    defined <- ask
                    addLogConditional (identifier `elem` defined)
                        ("Error: Undeclared identifier "++identifier)
checkDefined _ _ = return True

readMaybeStatement :: Maybe Statement -> Traverser (Defined, Bool)
readMaybeStatement Nothing = do
    defined <- ask
    return (defined, True)
readMaybeStatement (Just statement) = readStatement statement

readStatement :: Statement -> Traverser (Defined, Bool)
readStatement ( SAssign identifier@(EId i) expr) = do
    b <- readExpr expr
    let eMsg = "  .. found in assignment\n     \'"++ i ++ " = " ++ ppshow expr ++ "\'"
    addLogConditional b eMsg
    newDefined <- addVar Var identifier
    return (newDefined, b)

readStatement ( SExpr expr) = do
    b <- readExpr expr
    let eMsg = "  .. found in statement\n     \'"++ ppshow expr ++ "\'"
    addLogConditional b eMsg
    defined <- ask
    return (defined, b)

readStatement ( SBlock statements ) = do
    defined <- ask
    (_, b) <- readStatementsPropagate statements
    return (defined, b)

readStatement ( SWhile expr statement ) = do
    whileCorrect <- readExpr expr
    let eWhileMsg = "  .. found in while expression\n     \'while "++ ppshow expr ++ "\'"
    addLogConditional whileCorrect eWhileMsg
    (_, bodyCorrect) <- readStatement statement
    let eBodyMsg = "  .. found in while expression"
    addLogConditional bodyCorrect eBodyMsg
    defined <- ask
    return (defined, whileCorrect && bodyCorrect)

readStatement ( SIfElse expr ifStatement elseStatement ) = do
    let ifMsg = "  .. found in if statement\n     \'if "++ ppshow expr ++ "\'"
    ifExprCorrect <- readExpr expr
    addLogConditional ifExprCorrect ifMsg
    let bodyMsg = "  .. found in if statement"
    (_, bodyCorrect) <- readStatement ifStatement
    addLogConditional bodyCorrect bodyMsg
    let elseMsg = "  .. found in else statement"
    (_, elseBodyCorrect) <- readMaybeStatement elseStatement
    addLogConditional elseBodyCorrect elseMsg
    defined <- ask
    return (defined, ifExprCorrect && bodyCorrect && elseBodyCorrect)

readStatement ( SFuncDef identifier@(EId i) arguments body ) = do
    definedForFunc <- addArgs arguments
    (_, funcBodyCorrect)<- local (const definedForFunc) $ readStatement body
    let bodyMsg = "  .. found in definition of function \'" ++ i ++ "\'"
    addLogConditional funcBodyCorrect bodyMsg
    newDefined <- addVar Func identifier
    return (newDefined, funcBodyCorrect)
    where addArgs (a:rgs) = do
            newDefined <- addVar Var a
            local (const newDefined) $ addArgs rgs
          addArgs [] = ask

readExpr :: Expr -> Traverser Bool
readExpr (EFuncCall identifier args) = do
                    correctIdentifier <- checkDefined Func identifier
                    correctArgs <- checkArgs args
                    return $ correctIdentifier && correctArgs
        where checkArgs (a:rgs) = do 
                    correctArg <- readExpr a
                    correctArgs <- checkArgs rgs
                    return $ correctArg && correctArgs
              checkArgs [] = return True
readExpr (Expr operator leftOperand rightOperand) = do
                    readExpr leftOperand
                    readExpr rightOperand                                 
readExpr expr = checkDefined Var expr 

readStatementsPropagate :: [Statement] -> Traverser (Defined, Bool)
readStatementsPropagate [] = do
    defined <- ask
    return (defined, True)
readStatementsPropagate (statement : rest) = do
    (newDefined, statementCorrect) <- readStatement statement
    (_, restCorrect) <- local (const newDefined) (readStatementsPropagate rest)
    defined <- ask
    return (defined, statementCorrect && restCorrect)

readStatements :: [Statement] -> Traverser Defined
readStatements [] = ask
readStatements (statement : rest) = do
    (newDefined, _) <- readStatement statement
    local (const newDefined) (readStatements rest)
    return newDefined
