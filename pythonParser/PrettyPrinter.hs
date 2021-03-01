module PrettyPrinter where
import Text.PrettyPrint
import qualified Prelude as P
import SyntacticAnalysis

class Pretty a where
    pretty :: a -> Doc

ppshow :: Pretty a => a -> P.String
ppshow = renderStyle (style {lineLength = 80}) P.. pretty

pplistshow :: Pretty a => [a] -> P.String
pplistshow as = renderStyle (style {lineLength = 80}) P.. vcat P.$ P.map pretty as

prettyArgs [] = empty
prettyArgs [x] = pretty x
prettyArgs (x:xs) = pretty x <> comma <> space <> prettyArgs xs

withoutParens op l r = pretty l <+> text op <+> pretty r
parensDivOp op l r = parOpt l <+> text op <+> parOpt r
    where
        parOpt expr@(Expr op _ _)
            | op `P.elem` ["+", "-", "<", ">", "<=", ">=", "==", "!="] = parens P.$ pretty expr
            | P.otherwise = pretty expr
        parOpt x = pretty x
parensSumOp op l r = parOpt l <+> text op <+> parOpt r
    where
        parOpt expr@(Expr op _ _)
            | op `P.elem` ["-", "<", ">", "<=", ">=", "==", "!="] = parens P.$ pretty expr
            | P.otherwise = pretty expr       
        parOpt x = pretty x
chooseParensSystem op l r
    | op P.== "+" P.|| op P.== "-" = parensSumOp op l r
    | op P.== "*" P.|| op P.== "/" = parensDivOp op l r
    | P.otherwise = withoutParens op l r

instance Pretty Expr where
    pretty (Expr op l r) = chooseParensSystem op l r
    pretty (EInt i) = int i
    pretty (EStrLit str) = doubleQuotes P.$ text str
    pretty (EId i) = text i
    pretty (EFuncCall fname fargs) = pretty fname <> parens (prettyArgs fargs)

nestStms (SBlock stms) = nest 2 P.$ vcat (P.map pretty stms)
nestStms stmt = nest 2 P.$ vcat (P.map pretty [stmt])

instance Pretty Statement where
    pretty (SAssign i expr) = pretty i <+> equals <+> pretty expr <> semi
    pretty (SExpr expr) = pretty expr <> semi
    pretty block@(SBlock _) = lbrace $+$ nestStms block $$ rbrace
    pretty (SWhile expr stmt) = 
        vcat [text "while" <+> parens (pretty expr) <+> lbrace,
        nestStms stmt,
        rbrace]
    pretty (SIfElse expr ifstmt (P.Just elseStmt)) = 
        vcat [text "if" <+> parens (pretty expr) <+> lbrace,
              nestStms ifstmt,
              rbrace <+> text "else" <+> lbrace,
              nestStms elseStmt,
              rbrace]
    pretty (SIfElse expr ifstmt P.Nothing) =
        vcat [text "if" <+> parens (pretty expr) <+> lbrace,
              nestStms ifstmt,
              rbrace]
    pretty (SFuncDef i args body) =
        vcat [text "def" <+> pretty i <> parens (prettyArgs args) <+> lbrace,
              nestStms body,
              rbrace <+> text "\n"]