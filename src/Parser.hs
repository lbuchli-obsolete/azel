module Parser where

import Control.Applicative (many, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Error (ParseError)
import Text.Parsec (parse, parseTest, manyTill, many1, try, optional, notFollowedBy, eof)
import Text.Parsec.Prim ((<?>))
import Control.Monad (void)
import Data.Word (Word8)
import Data.Bits (rotate)
import Language

azParse :: String -> String -> Either ParseError AzProg
azParse = parse parseProg

testParse :: String -> IO ()
testParse = parseTest (parseAzFuncDecl <* eof)

parseProg :: Parser AzProg
parseProg = Prog <$> many (try parseAzItem) <* (eof <|> (many anyChar >>= fail))

parseAzItem :: Parser AzItem
parseAzItem
  =   try (FuncDecl <$> parseAzFuncDecl <?> "function declaration")
  <|> try (TypeDecl <$> parseAzTypeDecl <?> "type declaration")
  <|> ((skipLine <?> "comment line") *> parseAzItem)

parseAzFuncDecl :: Parser AzFuncDecl
parseAzFuncDecl = symbol "::" *> ((,) <$> parseAzFuncSignature <*> parseVariants)
  where parseVariants = many1 (indentNewline *> parseAzFuncVariant <* optional ws) <* optional newline

parseAzFuncSignature :: Parser AzFuncSignature
parseAzFuncSignature
  = try $ SAp <$> parseAzFuncSignatureNR <*> (symbol "=>" *> parseAzFuncSignatureNR)
  <|> parseAzFuncSignatureNR

-- Parse function signature without return (=>)
parseAzFuncSignatureNR :: Parser AzFuncSignature
parseAzFuncSignatureNR
  = mkApChain SAp <$> many1 parseAzFuncSignatureNAp

-- Parse function signature without application
parseAzFuncSignatureNAp :: Parser AzFuncSignature
parseAzFuncSignatureNAp
  = try (mkApChain SAp <$> ((:) <$> parseAzFuncSignatureNA <*> many (symbol "->" *> parseAzFuncSignatureNA)))
  <|> parseAzFuncSignatureNA
  
-- Parse function signature without arrow
parseAzFuncSignatureNA :: Parser AzFuncSignature
parseAzFuncSignatureNA = symbol "(" *> parseAzFuncSignatureNR <* symbol ")"
  <|> SSymbol <$> (string "'" *> manyTill anyNoSpace (symbol "'"))
  <|> notFollowedBy (symbol "->" <|> symbol "=>") *> (SType <$> parse_type)
  where
    parse_type
      = Concrete <$> (symbol "<" *> manyTill anyNoSpace (symbol ">"))
      <|> Polymorphic <$> many1 anyNoSpace <* optional ws

parseAzFuncVariant :: Parser AzFuncVariant
parseAzFuncVariant = (,) <$> var_decls <*> (symbol "=" *> parseAzExpr)
  where
    var_decls = many $ Right <$> (many1 (notFollowedBy (symbol "=") *> anyNoSpace) <* ws)

parseAzExpr :: Parser AzExpr
parseAzExpr = mkApChain EAp <$> many1 (parseAzExprNAp <* optional ws)

-- Parse expression without application
parseAzExprNAp :: Parser AzExpr
parseAzExprNAp
  =   try $ symbol "(" *> parseAzExpr <* symbol ")" <* optional ws
  <|> try parseELet
  <|> try parseECase
  <|> try (EByte <$> (string "0" *> hexNumber <* optional ws))
  <|> EVar <$> many1 anyNoSpace <* optional ws

parseELet :: Parser AzExpr
parseELet = symbol "let" *> undefined

parseECase :: Parser AzExpr
parseECase = symbol "match" *> undefined 

parseAzTypeDecl :: Parser AzTypeDecl
parseAzTypeDecl = symbol ":>" *> undefined -- TODO

symbol :: String -> Parser String
symbol s = string s <* optional ws

ws :: Parser ()
ws = void $ many1 (oneOf "\t ")

indentNewline :: Parser ()
indentNewline = optional ws *> newline *> ws

anyNoSpace :: Parser Char
anyNoSpace = noneOf "\n\t "

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
