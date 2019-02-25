{-# LANGUAGE ScopedTypeVariables #-}

module FlatBuffers.Compiler.Parser where

import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.Coerce                        (coerce)
import           Data.Functor
import           Data.List.NonEmpty
import           Data.Maybe                         (catMaybes)
import qualified Data.Text                          as T
import           Data.Void                          (Void)
import           FlatBuffers.Compiler.ParseTree
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

type Parser = Parsec Void String

-- | Roughly based on: https://google.github.io/flatbuffers/flatbuffers_grammar.html.
-- Differences between this parser and the above grammar:
-- 
--   * Unions members now support aliases.
--   * An enum's underlying type used to be optional (defaulting to @short@), but now it's mandatory.
--   * Attributes can be reffered to either as an identifier or as a string literal (e.g. @attr@ or @"attr"@)
schema :: Parser Schema
schema = do
  sc
  includes <- many include
  decls <- many (decl <|> failOnInclude)
  eof
  pure $ Schema includes (catMaybes decls)
  where
    failOnInclude = include *> fail "\"include\" statements must be at the beginning of the file."


decl :: Parser (Maybe Decl)
decl =
  choice
    [ Just . DeclN <$> namespaceDecl
    , Just . DeclT <$> typeDecl
    , Just . DeclE <$> enumDecl
    , Just . DeclU <$> unionDecl
    , Just . DeclR <$> rootDecl
    , Just . DeclFE <$> fileExtensionDecl
    , Just . DeclFI <$> fileIdentifierDecl
    , Just . DeclA <$> attributeDecl
    , Nothing <$ jsonObj
    , Nothing <$ rpcDecl
    ]

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

curly, square, parens :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")
square = between (symbol "[") (symbol "]")
parens = between (symbol "(") (symbol ")")


commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (symbol ",")

commaSep1 :: Parser a -> Parser (NonEmpty a)
commaSep1 p = NE.sepBy1 p (symbol ",")

semi, colon :: Parser String
semi = symbol ";"
colon = symbol ":"

ident :: Parser Ident
ident = label "identifier" $ (lexeme . try) identifier
  where
    identifier = fmap (Ident . T.pack) $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

typ :: Parser Type
typ =
  Tint8 <$ (symbol "int8" <|> symbol "byte") <|>
  Tint16 <$ (symbol "int16" <|> symbol "short") <|>
  Tint32 <$ (symbol "int32" <|> symbol "int") <|>
  Tint64 <$ (symbol "int64" <|> symbol "long") <|>
  Tword8 <$ (symbol "uint8" <|> symbol "ubyte") <|>
  Tword16 <$ (symbol "uint16" <|> symbol "ushort") <|>
  Tword32 <$ (symbol "uint32" <|> symbol "uint") <|>
  Tword64 <$ (symbol "uint64" <|> symbol "ulong") <|>

  Tfloat <$ (symbol "float32" <|> symbol "float") <|>
  Tdouble <$ (symbol "float64" <|> symbol "double") <|>

  Tbool <$ symbol "bool" <|>
  Tstring <$ symbol "string" <|>
  Tvector <$> label "array type" (vector typ) <|>
  Tident <$> label "type identifier" ident
  where
    vector = between (symbol "[" *> (notFollowedBy (symbol "[") <|> fail "nested vector types not supported" )) (symbol "]")

field :: Parser Field
field = do
  i <- ident
  colon
  t <- typ
  def <- optional (symbol "=" *> numberLiteral)
  md <- metadata
  semi
  pure $ Field i t def md

typeDecl :: Parser TypeDecl
typeDecl = do
  tt <- rword "table" $> Table <|> rword "struct" $> Struct
  i <- ident
  md <- metadata
  fs <- curly (NE.some field)
  pure $ TypeDecl tt i md fs

enumDecl :: Parser EnumDecl
enumDecl = do
  rword "enum"
  i <- ident
  colon
  t <- typ
  md <- metadata
  v <- curly (commaSep1 enumValDecl)
  pure $ EnumDecl i t md v

enumValDecl :: Parser EnumValDecl
enumValDecl = EnumValDecl <$> ident <*> optional (symbol "=" *> intLiteral)

unionDecl :: Parser UnionDecl
unionDecl = do
  rword "union"
  i <- ident
  md <- metadata
  v <- curly (commaSep1 unionValDecl)
  pure $ UnionDecl i md v

unionValDecl :: Parser UnionValDecl
unionValDecl = UnionValDecl <$> optional (try (ident <* colon)) <*> ident

namespaceDecl :: Parser NamespaceDecl
namespaceDecl = NamespaceDecl <$> (rword "namespace" *> NE.sepBy1 ident (symbol ".") <* semi)

stringLiteral :: Parser StringLiteral
stringLiteral =
  label "string literal" $
    fmap (StringLiteral . T.pack) . lexeme $
      char '"' >> manyTill L.charLiteral (char '"')

intLiteral :: Parser IntLiteral
intLiteral =
  label "integer literal" . lexeme $
    L.signed sc L.decimal

numberLiteral :: Parser NumberLiteral
numberLiteral = 
  label "number literal" . lexeme $ do
    (consumed, _n) <- match (L.signed sc L.scientific)
    pure (NumberLiteral consumed)

literal :: Parser Literal
literal = LiteralN <$> numberLiteral <|> LiteralS <$> stringLiteral

metadata :: Parser (Maybe Metadata)
metadata = label "metadata" . optional . parens . fmap Metadata . commaSep1 $
  (,) <$> attributeName <*> optional (colon *> literal)

include :: Parser Include
include = Include <$> (rword "include" *> stringLiteral <* semi)

rootDecl :: Parser RootDecl
rootDecl = RootDecl <$> (rword "root_type" *> ident <* semi)

fileExtensionDecl :: Parser FileExtensionDecl
fileExtensionDecl = FileExtensionDecl <$> (rword "file_extension" *> stringLiteral <* semi)

fileIdentifierDecl :: Parser FileIdentifierDecl
fileIdentifierDecl = FileIdentifierDecl <$> (rword "file_identifier" *> stringLiteral <* semi)

attributeDecl :: Parser AttributeDecl
attributeDecl = AttributeDecl <$> (rword "attribute" *> attributeName <* semi)

attributeName :: Parser Ident
attributeName = coerce stringLiteral <|> ident

jsonObj :: Parser ()
jsonObj =
  label "JSON object" (void jobject)
  where
    json = choice [void jstring, void jnumber, jbool, jnull, void jarray, void jobject]
    jnull = rword "null"
    jbool = rword "true" <|> rword "false"
    jstring = stringLiteral
    jnumber = numberLiteral
    jarray  = square (commaSep json)
    jobject = curly (commaSep keyValuePair)

    keyValuePair = do
      void stringLiteral <|> void ident
      colon
      json

rpcDecl :: Parser ()
rpcDecl = void $ rword "rpc_service" >> ident >> curly (NE.some rpcMethod)

rpcMethod :: Parser ()
rpcMethod = ident >> parens ident >> colon >> ident >> metadata >> void semi
