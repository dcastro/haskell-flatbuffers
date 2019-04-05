{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FlatBuffers.Internal.Compiler.Parser where

import           Control.Monad                            ( when )
import qualified Control.Monad.Combinators.NonEmpty       as NE
import           Control.Monad.Except                     ( MonadError, MonadIO, liftIO, throwError )
import           Control.Monad.State                      ( MonadState, execStateT, get, put )

import           Data.Coerce                              ( coerce )
import           Data.Foldable                            ( traverse_ )
import           Data.Functor
import           Data.List.NonEmpty                       ( NonEmpty )
import qualified Data.List.NonEmpty                       as NE
import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict                          as Map
import           Data.Maybe                               ( catMaybes )
import           Data.Text                                ( Text )
import qualified Data.Text                                as T
import           Data.Void                                ( Void )

import           FlatBuffers.Internal.Compiler.SyntaxTree

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer               as L

type Parser = Parsec Void String

parseSchemas :: (MonadIO m, MonadError String m) => FilePath -> m (FileTree Schema)
parseSchemas rootFilePath = do
  input <- liftIO $ readFile rootFilePath
  case parse schema rootFilePath input of
    Left err -> throwError (errorBundlePretty err)
    Right rootSchema -> do
      let importedFilePaths = T.unpack . coerce <$> includes rootSchema
      importedSchemas <- flip execStateT Map.empty $ traverse_ parseImportedSchema importedFilePaths
      pure $ FileTree
              { fileTreeFilePath = rootFilePath
              , fileTreeRoot     = rootSchema
              , fileTreeForest   = importedSchemas
              }
  where
    parseImportedSchema ::
         MonadState (Map FilePath Schema) m
      => MonadIO m
      => MonadError String m
      => FilePath -> m ()
    parseImportedSchema filePath = do
      importedSchemas <- get
      when (filePath /= rootFilePath && Map.notMember filePath importedSchemas) $ do
        input <- liftIO $ readFile filePath
        case parse schema filePath input of
          Left err -> throwError (errorBundlePretty err)
          Right importedSchema -> do
            put (Map.insert filePath importedSchema importedSchemas)
            traverse_ (parseImportedSchema . T.unpack . coerce) (includes importedSchema)

-- | Roughly based on: https://google.github.io/flatbuffers/flatbuffers_grammar.html.
-- Differences between this parser and the above grammar:
-- 
--   * Unions members now support aliases.
--   * An enum's underlying type used to be optional (defaulting to @short@), but now it's mandatory.
--   * Attributes can be reffered to either as an identifier or as a string literal (e.g. @attr@ or @"attr"@).
--   * Struct fields can't have default values.
--   * The grammar states that table/struct field defaults can only be scalars (integer/floating point constants),
--     when in reality, it could be also be a boolean or an enum identifier.
--   * The grammar says attribute values can be integers, floats or string literals.
--     Flatc only allows integers and string literals. To make things simpler, we decided to go with flatc's
--     approach and disallow floats.
--   * The grammar says namespaces must include at least one fragment, but an empty namespace
--     (i.e. @namespace ;@) is perfectly valid.
--   * This supports `native_include` statements
--     (see: https://google.github.io/flatbuffers/flatbuffers_guide_use_cpp.html#flatbuffers_cpp_object_based_api)
schema :: Parser Schema
schema = do
  sc
  includes <- catMaybes <$> many (Just <$> include <|> Nothing <$ nativeInclude)
  decls <- many (decl <|> failOnInclude)
  eof
  pure $ Schema includes (catMaybes decls)
  where
    failOnInclude =
      include *> fail "\"include\" statements must be at the beginning of the file."
      <|> (nativeInclude *> fail "\"native_include\" statements must be at the beginning of the file.")

decl :: Parser (Maybe Decl)
decl =
  choice
    [ Just . DeclN <$> namespaceDecl
    , Just . DeclT <$> tableDecl
    , Just . DeclS <$> structDecl
    , Just . DeclE <$> enumDecl
    , Just . DeclU <$> unionDecl
    , Just . DeclR <$> rootDecl
    , Just . DeclFI <$> fileIdentifierDecl
    , Just . DeclA <$> attributeDecl
    , Nothing <$ fileExtensionDecl
    , Nothing <$ jsonObj
    , Nothing <$ rpcDecl
    ]

-- | space consumer - this consumes and ignores any whitespace + comments
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
rword w = (lexeme . try) (string w *> notFollowedBy (alphaNumChar <|> char '_'))

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
  TInt8 <$ (rword "int8" <|> rword "byte") <|>
  TInt16 <$ (rword "int16" <|> rword "short") <|>
  TInt32 <$ (rword "int32" <|> rword "int") <|>
  TInt64 <$ (rword "int64" <|> rword "long") <|>
  TWord8 <$ (rword "uint8" <|> rword "ubyte") <|>
  TWord16 <$ (rword "uint16" <|> rword "ushort") <|>
  TWord32 <$ (rword "uint32" <|> rword "uint") <|>
  TWord64 <$ (rword "uint64" <|> rword "ulong") <|>

  TFloat <$ (rword "float32" <|> rword "float") <|>
  TDouble <$ (rword "float64" <|> rword "double") <|>

  TBool <$ rword "bool" <|>
  TString <$ rword "string" <|>
  label "type identifier" (TRef <$> typeRef) <|>
  label "vector type" vector
  where
    vector = TVector <$> between
              (symbol "[" *> (notFollowedBy (symbol "[") <|> fail "nested vector types not supported" ))
              (symbol "]")
              typ

typeRef :: Parser TypeRef
typeRef = do
  idents <- many (try (ident <* symbol "."))
  i <- ident
  pure $ TypeRef (Namespace (coerce idents)) i

tableField :: Parser TableField
tableField = do
  i <- ident
  colon
  t <- typ
  def <- optional (symbol "=" *> defaultVal)
  md <- metadata
  semi
  pure $ TableField i t def md

structField :: Parser StructField
structField = do
  i <- ident
  colon
  t <- typ
  md <- metadata
  semi
  pure $ StructField i t md

tableDecl :: Parser TableDecl
tableDecl = do
  rword "table"
  i <- ident
  md <- metadata
  fs <- curly (many tableField)
  pure $ TableDecl i md fs

structDecl :: Parser StructDecl
structDecl = do
  rword "struct"
  i <- ident
  md <- metadata
  fs <- curly (NE.some structField)
  pure $ StructDecl i md fs

enumDecl :: Parser EnumDecl
enumDecl = do
  rword "enum"
  i <- ident
  colon
  t <- typ
  md <- metadata
  v <- curly (commaSep1 enumVal)
  pure $ EnumDecl i t md v

enumVal :: Parser EnumVal
enumVal = EnumVal <$> ident <*> optional (symbol "=" *> intLiteral)

unionDecl :: Parser UnionDecl
unionDecl = do
  rword "union"
  i <- ident
  md <- metadata
  v <- curly (commaSep1 unionVal)
  pure $ UnionDecl i md v

unionVal :: Parser UnionVal
unionVal = UnionVal <$> optional (try (ident <* colon)) <*> typeRef

namespaceDecl :: Parser NamespaceDecl
namespaceDecl =
  NamespaceDecl . Namespace . coerce <$>
    (rword "namespace" *> sepBy ident (symbol ".") <* semi)

stringLiteral :: Parser StringLiteral
stringLiteral =
  label "string literal" $
    fmap (StringLiteral . T.pack) . lexeme $
      char '"' >> manyTill L.charLiteral (char '"')

intLiteral :: Parser IntLiteral
intLiteral =
  label "integer literal" . lexeme $
    L.signed sc L.decimal

attributeVal :: Parser AttributeVal
attributeVal = 
  choice
    [ AttrI . unIntLiteral <$> intLiteral
    , AttrS . unStringLiteral <$> stringLiteral
    ]

defaultVal :: Parser DefaultVal
defaultVal =
  choice
    [ DefaultBool True <$ rword "true"
    , DefaultBool False <$ rword "false"
    , DefaultNum <$> label "number literal" (lexeme (L.signed sc L.scientific))
    , DefaultRef <$> ident
    ]

metadata :: Parser Metadata
metadata =
  label "metadata"
    . fmap (Metadata . Map.fromList . maybe [] NE.toList)
    . optional
    . parens
    . commaSep1 $
  (,) <$> attributeName <*> optional (colon *> attributeVal)

include :: Parser Include
include = Include <$> (rword "include" *> stringLiteral <* semi)

-- | See: https://google.github.io/flatbuffers/flatbuffers_guide_use_cpp.html#flatbuffers_cpp_object_based_api
nativeInclude :: Parser ()
nativeInclude = void (rword "native_include" >> stringLiteral >> semi)

rootDecl :: Parser RootDecl
rootDecl = RootDecl <$> (rword "root_type" *> typeRef <* semi)

fileExtensionDecl :: Parser ()
fileExtensionDecl = void (rword "file_extension" *> stringLiteral <* semi)

fileIdentifierDecl :: Parser FileIdentifierDecl
fileIdentifierDecl = do
  rword "file_identifier"
  fi <- stringLiteral
  when (T.length (coerce fi) /= 4) $
    fail "file_identifier must be exactly 4 characters"
  semi
  pure (FileIdentifierDecl fi)

attributeDecl :: Parser AttributeDecl
attributeDecl = AttributeDecl <$> (rword "attribute" *> attributeName <* semi)

attributeName :: Parser Text
attributeName = coerce stringLiteral <|> coerce ident

jsonObj :: Parser ()
jsonObj =
  label "JSON object" (void jobject)
  where
    json = choice [void jstring, void jnumber, jbool, jnull, void jarray, void jobject]
    jnull = rword "null"
    jbool = rword "true" <|> rword "false"
    jstring = stringLiteral
    jnumber = lexeme $ L.signed sc L.scientific
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
