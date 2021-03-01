{-# LANGUAGE TypeFamilies #-}
module TokenParser
where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

type Tokenizer = Parsec Void String

data Tkn = TInt Int
         | TId String
         | TString String
         | TIndent Int
         | TNewline
         | TAssign
         | TDef
         | TWhile 
         | TIf 
         | TElse
         | TColon
         | TOpenBracket
         | TCloseBracket
         | TComma
         | TBlockStart
         | TBlockEnd
         | TDivProd Char
         | TSumDif Char
         | TCmp String
         | TEq String
    deriving (Show, Eq, Ord)

data Wrap a =
  Wrap String
    a
  deriving (Show, Eq)

showTkn :: Tkn -> String
showTkn = show

isNewLine :: Wrap Tkn -> Bool
isNewLine (Wrap _ TNewline) = True
isNewLine _ = False

newtype TokStream = TokStream {unTokStream :: [Wrap Tkn]} deriving Show

unWrap :: Wrap a -> a
unWrap (Wrap _ a) = a --zahodí obal

wrapToStr :: Wrap a -> String
wrapToStr (Wrap s _) = s --vrátí původní string

lexeme :: Tokenizer a -> Tokenizer a
lexeme = L.lexeme (void $ many (char ' '))
pKeyword keyword = lexeme (string keyword <* lookAhead (char ' ' <|> char '\n' <|> char ':'))

tokenize' :: Tokenizer (Wrap Tkn)
tokenize' = choice [
    Wrap "\n" TNewline <$ char '\n', -- newlines
    (Wrap <$> id <*> TIndent . length) <$> (some (char ' ') :: Tokenizer String), -- pIndentation
    (Wrap <$> id <*> TString) <$> lexeme ( char '\"' *> manyTill L.charLiteral (char '\"') ), --string
    try $ Wrap "def " TDef <$ pKeyword "def",
    try $ Wrap "if " TIf <$ pKeyword "if",
    try $ Wrap "else " TElse <$ pKeyword "else",
    try $ Wrap "while " TWhile <$ pKeyword "while", 
    Wrap "( " TOpenBracket <$ lexeme (char '('),
    Wrap " )" TCloseBracket <$ lexeme (char ')'),
    try $ (Wrap <$> id <*> TId) <$> lexeme (some (letterChar <|> char '_') <* notFollowedBy alphaNumChar), -- identifiers
    Wrap ", " TComma <$ lexeme (char ','),
    Wrap ": " TColon <$ lexeme (char ':'),
    (Wrap <$> (:[]) <*> TDivProd) <$> (lexeme (char '/') <|> lexeme (char '*')),
    (Wrap <$> (:[]) <*> TSumDif) <$> lexeme (char '+' <|> char '-'),
    (Wrap <$> id <*> TCmp) <$> lexeme (choice [string "<=", string ">=", string "<", string ">"]),
    (Wrap <$> id <*> TEq ) <$> lexeme (choice [string "==", string "!="]),
    Wrap "= " TAssign <$ lexeme (char '='),
    (Wrap <$> show <*> TInt . fromInteger) <$> (lexeme L.decimal <* notFollowedBy letterChar)    --signed ints
    ]

tokenize = runParser (TokStream <$> tokenizeIndented [0] False) --tohle spustí tokenizér

tokenizeIndented :: [Int] -> Bool -> Tokenizer [Wrap Tkn]
tokenizeIndented x newLine = parseEof <|> do
  tok <- tokenize'
  case unWrap tok of
    TNewline -> (tok :) <$> tokenizeIndented x True
    (TIndent y) -> decideOnIndent y []
    _           -> if not newLine then (tok :) <$> tokenizeIndented x False else decideOnIndent 0 [tok]
  where
    decideOnIndent y lst
          | null x || y > head x = ((Wrap "" TBlockStart : lst) ++) <$> tokenizeIndented (y:x) False
          | y == head x = (lst ++) <$> tokenizeIndented x False
          | y < head x && y `elem` x = 
              ((  map (const $ Wrap "" TBlockEnd) (filter (>y) x ) ++ lst) ++ )<$> tokenizeIndented (filter (<=y) x) False
          | otherwise = fail "Wrong indentation !"
    parseEof = do
      eof :: Tokenizer ()
      return $ map (const $ Wrap "" TBlockEnd) (tail x)

{- Zbytek kódu obsahuje instanci Stream [Wrap MToken], která funguje jako adaptér
 - mezi seznamy označkovaných tokenů a Megaparsecem. Pro String, Text a
 - ByteStringy existují podobné adaptéry. -}
instance Stream TokStream where
  type Token TokStream = Tkn -- typ tokenu který uvidí parsovací funkce
  type Tokens TokStream = [Tkn] -- typ pro segment tokenů
  tokenToChunk _ = (: []) -- nějaké konverzní funkce pro předchozí typy
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokStream (x:xs)) = Just (unWrap x, TokStream xs) -- extrahuje první token (bez tagu)
  take1_ _ = Nothing
  takeN_ n (TokStream l@(_:_)) =
    Just (map unWrap $ take n l, TokStream $ drop n l) -- tosamé pro chunk
  takeN_ _ _ = Nothing
  takeWhile_ f (TokStream l) =
    (map unWrap $ takeWhile (f . unWrap) l, TokStream $ dropWhile (f . unWrap) l)

{- Sem dám velmi tlustou čáru, pod ní číst už asi nemusíte (nechcete)
 -
 - *************************************************************************
 -
 - Zbytek instance existuje jen kvůli hezčímu vypisování chybových hlášek.
 - Konkrétně, z tokenů (z tagů z Wrap) rekonstruuje smysluplný kus původního kódu
 - (v našem případě řádek) a řekne megaparsecu, kde přesně se v něm nachází
 - problematický token (aby šel podtrhnout). Pro základní funkcionalitu je to
 - samozřejmě naprosto zbytečné, ale schopnost rozumně ukazovat chyby v kódu (a
 - ne jen na seznamu tokenů) je jedna ze základních vlastností dobrých parserů
 - a měla by být všude. -}
instance VisualStream TokStream where
  showTokens _ (a :| b) = intercalate ", " $ map showTkn (a : b)

instance TraversableStream TokStream where
  reachOffset o pst =
    let oo = pstateOffset pst
        otoks = unTokStream $ pstateInput pst
        lineEnds =
          filter (isNewLine . snd) $
          takeWhile ((<= o) . fst) $ zip [oo ..] otoks
        newo
          | null lineEnds = oo
          | otherwise = succ . fst . last $ lineEnds
        newtoks = drop (newo - oo) otoks
        line =
          case concatMap wrapToStr $ takeWhile (not . isNewLine) newtoks of
            "" -> "<empty line>"
            a -> convertTabs a
              where convertTabs = concatMap convertTab
                    convertTab '\t' = replicate (unPos $ pstateTabWidth pst) ' '
                    convertTab c = [c]
        sp = pstateSourcePos pst
        srcLine = mkPos $ unPos (sourceLine sp) + length lineEnds
        lineo = o - newo
        srcCol = mkPos . (+ 1) . length . concatMap wrapToStr . take lineo $ newtoks
        newSrcPos = sp { sourceLine = srcLine
                       , sourceColumn = srcCol }
     in ( Just line
        , pst
            { pstateInput = TokStream newtoks
            , pstateOffset = newo
            , pstateSourcePos = newSrcPos
            })
