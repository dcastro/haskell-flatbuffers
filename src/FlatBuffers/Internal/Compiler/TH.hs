{-# LANGUAGE TemplateHaskell #-}

module FlatBuffers.Internal.Compiler.TH where

import           Control.Monad                                   ( join )

import           Data.Int
import           Data.Text                                       ( Text )
import qualified Data.Text                                       as T
import           Data.Word

import           FlatBuffers.Internal.Compiler.NamingConventions
import           FlatBuffers.Internal.Compiler.SemanticAnalysis  ( SymbolTable(..) )
import           FlatBuffers.Internal.Compiler.SyntaxTree        ( HasIdent(..), Ident(..), Namespace(..) )
import           FlatBuffers.Internal.Compiler.ValidSyntaxTree
import           FlatBuffers.Write

import           Language.Haskell.TH

-- TODO: process isRoot
compileSymbolTable :: SymbolTable EnumDecl StructDecl TableDecl UnionDecl -> Q [Dec]
compileSymbolTable symbols =
  join <$> traverse genTable (allTables symbols)

genTable :: (Namespace, TableDecl) -> Q [Dec]
genTable (_, table) = do
  let tableName = mkNameFor dataTypeName table
  (consSig, cons) <- genTableConstructor tableName table
  pure
    [ DataD [] tableName [] Nothing [] []
    , consSig
    , cons
    ]

genTableConstructor :: Name -> TableDecl -> Q (Dec, Dec)
genTableConstructor tableName table = do
  (argTypes, pats, exps, whereBindings) <- mconcat <$> traverse genTableContructorField (tableFields table)

  let retType = AppT (ConT ''WriteTable) (ConT tableName)
  let sig = foldr (\t accum -> ArrowT `AppT` t `AppT` accum) retType argTypes

  let consName = mkNameFor dataTypeConstructor table
  let consSig = SigD consName sig

  let body = NormalB $ AppE (VarE 'writeTable) (ListE exps)

  let cons =
        FunD consName
          [ Clause pats body whereBindings
          ]

  pure (consSig, cons)


genTableContructorField :: TableField -> Q ([Type], [Pat], [Exp], [Dec])
genTableContructorField tf =
  if tableFieldDeprecated tf
    then
      case tableFieldType tf of
        TUnion _ _           -> pure ([], [], [VarE 'deprecated, VarE 'deprecated], [])
        TVector _ (VUnion _) -> pure ([], [], [VarE 'deprecated, VarE 'deprecated], [])
        _                    -> pure ([], [], [VarE 'deprecated], [])
    else do
      let name = mkNameFor term tf

      let pat = VarP name

      typ <-
        case tableFieldType tf of
          TInt8   _   -> pure $ AppT (ConT ''Maybe) (ConT ''Int8)
          TInt16  _   -> pure $ AppT (ConT ''Maybe) (ConT ''Int16)
          TInt32  _   -> pure $ AppT (ConT ''Maybe) (ConT ''Int32)
          TInt64  _   -> pure $ AppT (ConT ''Maybe) (ConT ''Int64)
          TWord8  _   -> pure $ AppT (ConT ''Maybe) (ConT ''Word8)
          TWord16 _   -> pure $ AppT (ConT ''Maybe) (ConT ''Word16)
          TWord32 _   -> pure $ AppT (ConT ''Maybe) (ConT ''Word32)
          TWord64 _   -> pure $ AppT (ConT ''Maybe) (ConT ''Word64)
          TFloat  _   -> pure $ AppT (ConT ''Maybe) (ConT ''Float)
          TDouble _   -> pure $ AppT (ConT ''Maybe) (ConT ''Double)
          TBool   _   -> pure $ AppT (ConT ''Maybe) (ConT ''Bool)
          TString req -> pure $ requiredType req (ConT ''Text)

          _ -> undefined

      exps <-
        case tableFieldType tf of
          TInt8   (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'int8   ) (VarE name)
          TInt16  (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'int16  ) (VarE name)
          TInt32  (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'int32  ) (VarE name)
          TInt64  (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'int64  ) (VarE name)
          TWord8  (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'word8  ) (VarE name)
          TWord16 (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'word16 ) (VarE name)
          TWord32 (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'word32 ) (VarE name)
          TWord64 (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'word64 ) (VarE name)
          TFloat  (DefaultVal n) -> expForScalar (LitE . RationalL $ toRational n)        (VarE 'float  ) (VarE name)
          TDouble (DefaultVal n) -> expForScalar (LitE . RationalL $ toRational n)        (VarE 'double ) (VarE name)
          TBool   (DefaultVal b) -> expForScalar (if b then ConE 'True else ConE 'False)  (VarE 'bool   ) (VarE name)
          TString req            -> pure . pure $ AppE (requiredExp req (VarE 'text)) (VarE name)

          _ -> undefined

      pure $ ([typ], [pat], exps, [])

  where
    expForScalar :: Exp -> Exp -> Exp -> Q [Exp]
    expForScalar defaultValExp writeExp varExp = pure
      [ VarE 'optionalDef `AppE` defaultValExp `AppE` (VarE 'inline `AppE` writeExp) `AppE` varExp
      ]

requiredExp :: Required -> Exp -> Exp
requiredExp Req exp = exp
requiredExp Opt exp = AppE (VarE 'optional) exp

requiredType :: Required -> Type -> Type
requiredType Req t = t
requiredType Opt t = AppT (ConT ''Maybe) t

mkName' :: HasIdent a => a -> Name
mkName' = mkName . T.unpack . unIdent . getIdent

newName' :: HasIdent a => a -> Q Name
newName' = newName . T.unpack . unIdent . getIdent

mkNameFor :: HasIdent a => (Text -> Text) -> a -> Name
mkNameFor f = mkName . T.unpack . f . unIdent . getIdent
