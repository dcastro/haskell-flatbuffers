module Parser where

import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.Functor
import           Data.List.NonEmpty
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Void                          (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

type Parser = Parsec Void String

data Schema = Schema
  { namespaces :: [Namespace]
  , typeDecls  :: [TypeDecl]
  }

newtype Ident = Ident { unIdent :: Text }
  deriving Show

newtype Namespace = Namespace { unNamespace :: [Ident] }
  deriving Show

data TypeDecl = TypeDecl { typeDeclType :: TypeDeclType, typeIdent :: Ident, typeFields :: NonEmpty Field }
  deriving Show

data TypeDeclType = Table | Struct
  deriving Show

data Field = Field { fieldIdent :: Ident, fieldType :: Type }
  deriving Show

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
  deriving (Show)

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
    identifier = fmap (Ident . T.pack) $ (:) <$> letterChar <*> many alphaNumChar

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
  Vector <$> vector typ <|>
  Tident <$> ident
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
