{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.Internal.Compiler.TH where

import           Control.Monad                                   ( join )

import           Data.Int
import           Data.Text                                       ( Text )
import qualified Data.Text                                       as T
import           Data.Word

import qualified FlatBuffers.Internal.Compiler.NamingConventions as NC
import           FlatBuffers.Internal.Compiler.SemanticAnalysis  ( SymbolTable(..) )
import           FlatBuffers.Internal.Compiler.SyntaxTree        ( HasIdent(..), Ident(..), Namespace(..), TypeRef(..) )
import           FlatBuffers.Internal.Compiler.ValidSyntaxTree
import           FlatBuffers.Write
import           FlatBuffers.Read

import           Language.Haskell.TH

-- | Helper method to create function types.
-- @ConT ''Int ~> ConT ''String === Int -> String@
(~>) :: Type -> Type -> Type
a ~> b = ArrowT `AppT` a `AppT` b
infixr 1 ~>

-- TODO: process isRoot
compileSymbolTable :: SymbolTable EnumDecl StructDecl TableDecl UnionDecl -> Q [Dec]
compileSymbolTable symbols =
  join <$> traverse genTable (allTables symbols)

genTable :: (Namespace, TableDecl) -> Q [Dec]
genTable (_, table) = do
  tableName <- newNameFor NC.dataTypeName table
  (consSig, cons) <- genTableConstructor tableName table

  let getters = foldMap (genGetter tableName table) (tableFields table)

  pure $
    [ DataD [] tableName [] Nothing [] []
    , consSig
    , cons
    ] <> getters

genTableConstructor :: Name -> TableDecl -> Q (Dec, Dec)
genTableConstructor tableName table = do
  (argTypes, pats, exps, whereBindings) <- mconcat <$> traverse genTableContructorField (tableFields table)

  let retType = AppT (ConT ''WriteTable) (ConT tableName)
  let sig = foldr (~>) retType argTypes

  let consName = mkNameFor NC.dataTypeConstructor table
  let consSig = SigD consName sig

  let body = NormalB $ AppE (VarE 'writeTable) (ListE exps)

  let cons =
        FunD consName
          [ Clause pats body whereBindings
          ]

  pure (consSig, cons)

genGetter :: Name -> TableDecl -> TableField -> [Dec]
genGetter tableName table tf =
  if tableFieldDeprecated tf
    then []
    else
      case tableFieldType tf of
        TWord8 (DefaultVal n)   -> [ mkSig $ ConT ''Word8,  fun (bodyForScalar (LitE . IntegerL . toInteger $ n)   (VarE 'readWord8)) ]
        TWord16 (DefaultVal n)  -> [ mkSig $ ConT ''Word16, fun (bodyForScalar (LitE . IntegerL . toInteger $ n)   (VarE 'readWord16)) ]
        TWord32 (DefaultVal n)  -> [ mkSig $ ConT ''Word32, fun (bodyForScalar (LitE . IntegerL . toInteger $ n)   (VarE 'readWord32)) ]
        TWord64 (DefaultVal n)  -> [ mkSig $ ConT ''Word64, fun (bodyForScalar (LitE . IntegerL . toInteger $ n)   (VarE 'readWord64)) ]
        TInt8 (DefaultVal n)    -> [ mkSig $ ConT ''Int8,   fun (bodyForScalar (LitE . IntegerL . toInteger $ n)   (VarE 'readInt8)) ]
        TInt16 (DefaultVal n)   -> [ mkSig $ ConT ''Int16,  fun (bodyForScalar (LitE . IntegerL . toInteger $ n)   (VarE 'readInt16)) ]
        TInt32 (DefaultVal n)   -> [ mkSig $ ConT ''Int32,  fun (bodyForScalar (LitE . IntegerL . toInteger $ n)   (VarE 'readInt32)) ]
        TInt64 (DefaultVal n)   -> [ mkSig $ ConT ''Int64,  fun (bodyForScalar (LitE . IntegerL . toInteger $ n)   (VarE 'readInt64)) ]
        TFloat (DefaultVal n)   -> [ mkSig $ ConT ''Float,  fun (bodyForScalar (LitE . RationalL . toRational $ n) (VarE 'readFloat)) ]
        TDouble (DefaultVal n)  -> [ mkSig $ ConT ''Double, fun (bodyForScalar (LitE . RationalL . toRational $ n) (VarE 'readDouble)) ]
        TBool (DefaultVal b)    -> [ mkSig $ ConT ''Bool,   fun (bodyForScalar (if b then ConE 'True else ConE 'False) (VarE 'readBool)) ]
        TString req ->
          [ mkSig $ requiredType req $ ConT ''Text
          , fun (bodyForNonScalar req (VarE 'readText))
          ]
        TTable tref req ->
          [ mkSig $ requiredType req $ ConT ''Table `AppT` typeRefAsType tref
          , fun (bodyForNonScalar req (VarE 'readTable))
          ]

        _ -> []
  where
    funName = mkName (T.unpack (NC.getter table tf))
    fieldIndex = LitE (IntegerL (tableFieldId tf))
    fun body = FunD funName [ Clause [] (NormalB body) [] ]

    bodyForNonScalar req readExp =
      case req of
        Req ->
          foldl1 AppE
            [ VarE 'readTableFieldReq
            , readExp
            , fieldIndex
            , LitE (StringL (T.unpack . unIdent . getIdent $ tf))
            ]
        Opt ->
          foldl1 AppE
            [ VarE 'readTableFieldOpt
            , readExp
            , fieldIndex
            ]

    bodyForScalar defaultValExp readExp =
      foldl1 AppE
        [ VarE 'readTableFieldWithDef
        , readExp
        , fieldIndex
        , defaultValExp
        ]

    mkSig typ =
      SigD (mkName (T.unpack (NC.getter table tf))) $
          ForallT [PlainTV (mkName "m")] [ConT ''ReadCtx `AppT` VarT (mkName "m")] $
            ConT ''Table `AppT` ConT tableName ~> VarT (mkName "m") `AppT` typ

genTableContructorField :: TableField -> Q ([Type], [Pat], [Exp], [Dec])
genTableContructorField tf =
  if tableFieldDeprecated tf
    then
      case tableFieldType tf of
        TUnion _ _           -> pure ([], [], [VarE 'deprecated, VarE 'deprecated], [])
        TVector _ (VUnion _) -> pure ([], [], [VarE 'deprecated, VarE 'deprecated], [])
        _                    -> pure ([], [], [VarE 'deprecated], [])
    else do
      name <- newNameFor NC.term tf

      let arg = VarP name
      let argRef = VarE name

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
          TString req        -> pure $ requiredType req (ConT ''Text)
          TTable typeRef req -> pure $ requiredType req (ConT ''WriteTable `AppT` typeRefAsType typeRef)

          _ -> undefined

      exps <-
        case tableFieldType tf of
          TInt8   (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'int8   ) argRef
          TInt16  (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'int16  ) argRef
          TInt32  (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'int32  ) argRef
          TInt64  (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'int64  ) argRef
          TWord8  (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'word8  ) argRef
          TWord16 (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'word16 ) argRef
          TWord32 (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'word32 ) argRef
          TWord64 (DefaultVal n) -> expForScalar (LitE . IntegerL $ toInteger n)          (VarE 'word64 ) argRef
          TFloat  (DefaultVal n) -> expForScalar (LitE . RationalL $ toRational n)        (VarE 'float  ) argRef
          TDouble (DefaultVal n) -> expForScalar (LitE . RationalL $ toRational n)        (VarE 'double ) argRef
          TBool   (DefaultVal b) -> expForScalar (if b then ConE 'True else ConE 'False)  (VarE 'bool   ) argRef
          TString req            -> pure . pure $ AppE (requiredExp req (VarE 'text)) argRef
          TTable _ req           -> pure . pure $ AppE (requiredExp req (VarE 'unWriteTable)) argRef

          _ -> undefined

      pure $ ([typ], [arg], exps, [])

  where
    expForScalar :: Exp -> Exp -> Exp -> Q [Exp]
    expForScalar defaultValExp writeExp varExp = pure
      [ VarE 'optionalDef `AppE` defaultValExp `AppE` (VarE 'inline `AppE` writeExp) `AppE` varExp
      ]


typeRefAsType :: TypeRef -> Type
typeRefAsType (TypeRef "" ident) =
  ConT (mkNameFor NC.dataTypeName ident)
typeRefAsType (TypeRef ns (Ident ident)) =
  ConT . mkName . T.unpack $ NC.namespace ns <> "." <> NC.dataTypeName ident

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

newNameFor :: HasIdent a => (Text -> Text) -> a -> Q Name
newNameFor f = newName . T.unpack . f . unIdent . getIdent
