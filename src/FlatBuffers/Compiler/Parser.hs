{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module FlatBuffers.Compiler.Parser where

import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.Functor
import           Data.List.NonEmpty
import           Data.String                        (IsString)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Void                          (Void)
import           Data.Word
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

type Parser = Parsec Void String

-- | Roughly based on: https://google.github.io/flatbuffers/flatbuffers_grammar.html
-- Differences between this parser and the above grammar:
-- * Unions members now support aliases.
-- * Enums used to be have a default underlying type (short), but now it must be specified by the user.
data Schema = Schema
  { includes :: [Include]
  , typeDecls  :: [TypeDecl]
  , enumDecls :: [EnumDecl]
  }
  deriving (Show, Eq)

instance Semigroup Schema where
  Schema i1 t1 e1 <> Schema i2 t2 e2 =
    Schema (i1 <> i2) (t1 <> t2) (e1 <> e2)

instance Monoid Schema where
  mempty = Schema [] [] []

newtype Ident = Ident { unIdent :: Text }
  deriving (Show, Eq, IsString)

newtype Include = Include { unInclude :: StringLiteral }
  deriving (Show, Eq, IsString)

newtype StringLiteral = StringLiteral { unStringLiteral :: Text }
  deriving (Show, Eq, IsString)

newtype IntLiteral = IntLiteral { unIntLiteral :: Integer }
  deriving (Show, Eq, Num, Enum, Ord, Real, Integral)

newtype NumberLiteral = NumberLiteral { unNumberLiteral :: String }
  deriving (Show, Eq, IsString)

data Literal = LiteralN NumberLiteral | LiteralS StringLiteral
  deriving (Show, Eq)

newtype Metadata = Metadata { unMetadata :: NonEmpty (Ident, Maybe Literal)}
  deriving (Show, Eq)

newtype Namespace = Namespace { unNamespace :: NonEmpty Ident }
  deriving (Show, Eq)

data TypeDecl = TypeDecl
  { typeDeclType :: TypeDeclType
  , typeIdent    :: Ident
  , typeMetadata :: Maybe Metadata
  , typeFields   :: NonEmpty Field
  } deriving (Show, Eq)

data TypeDeclType = Table | Struct
  deriving (Show, Eq)

data Field = Field
  { fieldIdent    :: Ident
  , fieldType     :: Type
  , fieldDefault  :: Maybe NumberLiteral
  , fieldMetadata :: Maybe Metadata
  } deriving (Show, Eq)

data EnumDecl = EnumDecl
  { enumDeclIdent    :: Ident
  , enumDeclType     :: Type
  , enumDeclMetadata :: Maybe Metadata
  , enumDeclVals     :: NonEmpty EnumValDecl
  }
  deriving (Show, Eq)

data EnumValDecl = EnumValDecl
  { enumValDeclIdent   :: Ident
  , enumValDeclLiteral :: Maybe IntLiteral
  } deriving (Show, Eq)

data Type
  -- numeric
  =  Tint8
  | Tint16
  | Tint32
  | Tint64
  | Tword8
  | Tword16
  | Tword32
  | Tword64
  -- floating point
  | Tfloat
  | Tdouble
  -- others
  | Tbool
  | Tstring
  | Tvector Type
  | Tident Ident
  deriving (Show, Eq)

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

curly, parens :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser (NonEmpty a)
commaSep p = NE.sepBy1 p (symbol ",")

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
  v <- curly (commaSep enumValDecl)
  pure $ EnumDecl i t md v

enumValDecl :: Parser EnumValDecl
enumValDecl = EnumValDecl <$> ident <*> optional (symbol "=" *> intLiteral)

namespace :: Parser Namespace
namespace = Namespace <$> (rword "namespace" *> NE.sepBy1 ident (symbol ".") <* semi)

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
metadata = label "metadata" . optional . parens . fmap Metadata . commaSep $
  (,) <$> ident <*> optional (colon *> literal)

include :: Parser Include
include = Include <$> (rword "include" *> stringLiteral <* semi)

schema :: Parser Schema
schema = do
  sc
  includes <- many include
  schemas <-
    many
      ((\x -> Schema [] [] []) <$> namespace <|>
       (\x -> Schema [] [x] []) <$> typeDecl <|>
       (\x -> Schema [] [] [x]) <$> enumDecl <|>
       include *> fail "\"include\" statements must be at the beginning of the file."
       )
  eof
  pure $ (mconcat schemas) { includes = includes }
