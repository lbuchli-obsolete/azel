module Parser where

import Control.Applicative (many, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Error()
import Text.Parsec (parse, manyTill, many1, try, optional, notFollowedBy, eof, choice)
import Text.Parsec.Prim ((<?>))
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Word (Word8)
import Data.Bits (rotate)
import Language
import Util

-- azParse :: String -> String -> Either ParseError L1
-- azParse = parse parseProg

-- testParse :: String -> IO ()
-- testParse = parseTest (parseL1FuncDecl <* optional (symbol "\n") <* eof)

parseSource :: SourceName -> Translator L0 L1
parseSource name = first (Error . show) $ parse parseProg name $ toStr
  where toStr (L0Prog str) = str

parseProg :: Parser L1
parseProg = L1Prog <$> many (try parseL1Item) <* (eof <|> (many anyChar >>= fail))

parseL1Item :: Parser L1Item
parseL1Item
  =   try (FuncDecl <$> parseL1FuncDecl <?> "function declaration")
  <|> try (TypeDecl <$> parseL1TypeDecl <?> "type declaration")
  <|> ((skipLine <?> "comment line") *> parseL1Item)

parseL1FuncDecl :: Parser L1FuncDecl
parseL1FuncDecl = symbol "::" *> ((,) <$> (parseL1FuncSignature <?> "function signature") <*> parseVariants)
  where parseVariants = many1 (try indentNewline *> parseL1FuncVariant <?> "function variant") <* optional newline

parseL1FuncSignature :: Parser L1FuncSignature
parseL1FuncSignature
  = try $ SAp <$> parseL1FuncSignatureNR <*> (symbol "=>" *> parseL1FuncSignatureNR)
  <|> parseL1FuncSignatureNR

-- Parse function signature without return (=>)
parseL1FuncSignatureNR :: Parser L1FuncSignature
parseL1FuncSignatureNR
  = mkApChain SAp <$> many1 parseL1FuncSignatureNAp

-- Parse function signature without application
parseL1FuncSignatureNAp :: Parser L1FuncSignature
parseL1FuncSignatureNAp
  = try (mkApChain SAp <$> ((:) <$> parseL1FuncSignatureNA <*> many (symbol "->" *> parseL1FuncSignatureNA)))
  <|> parseL1FuncSignatureNA
  
-- Parse function signature without arrow
parseL1FuncSignatureNA :: Parser L1FuncSignature
parseL1FuncSignatureNA = symbol "(" *> parseL1FuncSignatureNR <* symbol ")"
  <|> SSymbol <$> (string "'" *> manyTill anyNoSpace (symbol "'"))
  <|> notFollowedBy (symbol "->" <|> symbol "=>") *> (SType <$> parse_type)
  where
    parse_type
      = Polymorphic <$> anyAlpha <* optional ws
      <|> Concrete <$> anyNotSymbol ["(", ")"]

parseL1FuncVariant :: Parser L1FuncVariant
parseL1FuncVariant = (,) <$> var_decls <*> (symbol "=" *> parseL1Expr)
  where
    var_decls = many $ Right <$> anyNotSymbol ["="]

parseL1Expr :: Parser L1Expr
parseL1Expr = mkApChain EAp <$> many1 (parseL1ExprNAp <* optional ws) <?> "expression"

-- Parse expression without application
parseL1ExprNAp :: Parser L1Expr
parseL1ExprNAp
  =   try $ symbol "(" *> parseL1Expr <* symbol ")" <* optional ws
  <|> try parseELet
  <|> try parseECase
  <|> try (EByte <$> (string "0" *> hexNumber <* optional ws))
  <|> EVar <$> anyNotSymbol []

parseELet :: Parser L1Expr
parseELet = symbol "let" *> undefined

parseECase :: Parser L1Expr
parseECase = symbol "match" *> undefined 

parseL1TypeDecl :: Parser L1TypeDecl
parseL1TypeDecl = symbol ":>" *> undefined -- TODO

symbol :: String -> Parser String
symbol s = string s <* optional ws

ws :: Parser ()
ws = void $ many1 (oneOf "\t ")

indentNewline :: Parser ()
indentNewline = optional ws *> newline *> ws

anyNoSpace :: Parser Char
anyNoSpace = noneOf "\n\t "

anyNotSymbol :: [String] -> Parser String
anyNotSymbol nots = checkNots *> many1 anyNoSpace <* optional ws
  where
    checkNots = notFollowedBy (choice (map string nots)) <|> void (string "\\")

anyAlpha :: Parser Char
anyAlpha = oneOf "abcdefghijklmnopqrstuvwxyz"

skipLine :: Parser String
skipLine = manyTill anyChar newline

hexNumber :: Parser Word8
hexNumber = string "0x" *> ((+) <$> ((`rotate` 4) <$> nibble) <*> nibble)
  where
    nibble = index_of hex_nrs <$> oneOf hex_nrs
    hex_nrs = ['0'..'9'] ++ ['a'..'f']
    index_of (x:_) ch | x == ch = 0
    index_of (_:xs) ch = 1 + index_of xs ch
    index_of _ _ = error "Invalid hex digit"

mkApChain :: (a -> a -> a) -> [a] -> a
mkApChain f (x:xs) | not (null xs) = f x (mkApChain f xs)
mkApChain _ (x:xs) | null xs       = x
mkApChain _ _                      = error "Empty expression"
