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
  = Tbool
  | Tbyte
  | Tubyte
  | Tshort
  | Tushort
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
  symbol "bool" $> Tbool <|>
  symbol "byte" $> Tbyte <|>
  symbol "ubyte" $> Tubyte <|>
  symbol "short" $> Tshort <|>
  symbol "ushort" $> Tushort

field :: Parser Field
field = (Field <$> ident <*> (symbol ":" >> typ)) <* semi

typeDecl :: Parser TypeDecl
typeDecl = do
  tt <- symbol "table" $> Table <|> symbol "struct" $> Struct
  i <- ident
  fs <- curly (NE.some field)
  pure $ TypeDecl tt i fs
