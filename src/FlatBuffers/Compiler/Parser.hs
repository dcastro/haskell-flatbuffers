{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FlatBuffers.Compiler.Parser where

import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.Functor
import           Data.List.NonEmpty
import           Data.String                        (IsString)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Void                          (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

type Parser = Parsec Void String

-- | https://google.github.io/flatbuffers/flatbuffers_grammar.html
data Schema = Schema
  { includes :: [Include]
  , typeDecls  :: [TypeDecl]
  }
  deriving (Show, Eq)

instance Semigroup Schema where
  (<>) = mappend

instance Monoid Schema where
  mempty = Schema [] []
  Schema i t `mappend` Schema i2 t2 = Schema (i <> i2) (t <> t2)

newtype Ident = Ident { unIdent :: Text }
  deriving (Show, Eq, IsString)

newtype Include = Include { unInclude :: StringConst }
  deriving (Show, Eq, IsString)

newtype StringConst = StringConst { unStringConst :: Text }
  deriving (Show, Eq, IsString)

newtype Namespace = Namespace { unNamespace :: NonEmpty Ident }
  deriving (Show, Eq)

data TypeDecl = TypeDecl { typeDeclType :: TypeDeclType, typeIdent :: Ident, typeFields :: NonEmpty Field }
  deriving (Show, Eq)

data TypeDeclType = Table | Struct
  deriving (Show, Eq)

data Field = Field { fieldIdent :: Ident, fieldType :: Type }
  deriving (Show, Eq)

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
  | Vector Type
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

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

semi :: Parser String
semi = symbol ";"

ident :: Parser Ident
ident = (lexeme . try) identifier
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
  Vector <$> label "array type" (vector typ) <|>
  Tident <$> label "type identifier" ident
  where
    vector = between (symbol "[" *> (notFollowedBy (symbol "[") <|> fail "nested vector types not supported" )) (symbol "]")

field :: Parser Field
field = (Field <$> ident <*> (symbol ":" >> typ)) <* semi

typeDecl :: Parser TypeDecl
typeDecl = do
  tt <- rword "table" $> Table <|> rword "struct" $> Struct
  i <- ident
  fs <- curly (NE.some field)
  pure $ TypeDecl tt i fs

namespace :: Parser Namespace
namespace = Namespace <$> (rword "namespace" *> NE.sepBy1 ident (symbol ".") <* semi)

stringConst :: Parser StringConst
stringConst =
  fmap (StringConst . T.pack) . lexeme $
    char '"' >> manyTill L.charLiteral (char '"')

include :: Parser Include
include = Include <$> (rword "include" *> stringConst <* semi)

schema :: Parser Schema
schema = do
  sc
  includes <- many include
  schemas <-
    many
      ((\x -> Schema [] []) <$> namespace <|>
       (\x -> Schema [] [x]) <$> typeDecl <|>
       include *> fail "\"include\" statements must be at the beginning of the file."
       )
  eof
  pure $ (mconcat schemas) { includes = includes }
