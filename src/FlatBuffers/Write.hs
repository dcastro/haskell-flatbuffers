{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module FlatBuffers.Write
  ( WriteUnion(..)
  , WriteTable(..)
  , WriteStruct(..)
  , WriteVector(..)
  , encode
  , encodeWithFileIdentifier
  , encodeWithFileIdentifier'
  , vector
  , none
  , writeTable
  , writeUnion
  , writeStruct
  , writeVector
  , writeUnionType
  , writeUnionValue
  , writeUnionVectorOpt
  , writeUnionVectorReq
  , deprecated
  , optional
  , optionalDef
  , W.padded
  , W.inline
  , W.word8
  , W.word16
  , W.word32
  , W.word64
  , W.int8
  , W.int16
  , W.int32
  , W.int64
  , W.float
  , W.double
  , W.bool
  , W.text
  ) where

import           Data.Bifunctor             ( bimap )
import qualified Data.ByteString.Lazy       as BSL
import           Data.Int
import           Data.List.NonEmpty         ( NonEmpty )
import           Data.Text                  ( Text )
import           Data.Word

import           FlatBuffers.FileIdentifier ( FileIdentifier, HasFileIdentifier(..) )
import           FlatBuffers.Internal.Write ( Field(..), InlineField(..) )
import qualified FlatBuffers.Internal.Write as W
import           FlatBuffers.Types

encode :: WriteTable a -> BSL.ByteString
encode (WriteTable table) = W.root table

encodeWithFileIdentifier :: forall a. HasFileIdentifier a => WriteTable a -> BSL.ByteString
encodeWithFileIdentifier = encodeWithFileIdentifier' (getFileIdentifier @a)

encodeWithFileIdentifier' :: forall a. FileIdentifier -> WriteTable a -> BSL.ByteString
encodeWithFileIdentifier' fi (WriteTable table) = W.rootWithFileIdentifier fi table

newtype WriteTable a = WriteTable { unWriteTable :: Field }

newtype WriteStruct a = WriteStruct { unWriteStruct :: InlineField }

data WriteVector a = forall t. Traversable t => WriteVector !(t a)

vector :: forall t a. Traversable t => t a -> WriteVector a
vector = WriteVector

data WriteUnion a
  = None
  | Some !(Word8, Field)

writeTable :: [Field] -> WriteTable a
writeTable = WriteTable . W.table

writeStruct :: Alignment -> NonEmpty InlineField -> WriteStruct a
writeStruct structAlign xs = WriteStruct (W.struct structAlign xs)

writeUnion :: Word8 -> WriteTable a -> WriteUnion b
writeUnion n (WriteTable t) = Some (n, t)

none :: WriteUnion a
none = None

deprecated :: Field
deprecated = W.missing

optional :: (a -> Field) -> Maybe a -> Field
optional = maybe W.missing

optionalDef :: Eq a => a -> (a -> Field) -> (Maybe a -> Field)
optionalDef dflt write ma =
  case ma of
    Just a | a /= dflt -> write a
    _                  -> W.missing

writeVector :: (a -> Field) -> (WriteVector a -> Field)
writeVector write (WriteVector xs) =
  W.vector (write <$> xs)

writeUnionType :: WriteUnion a -> Field
writeUnionType !a =
  case a of
    None        -> W.missing
    Some (t, _) -> W.inline W.word8 t

writeUnionValue :: WriteUnion a -> Field
writeUnionValue !a =
  case a of
    None        -> W.missing
    Some (_, v) -> v

writeUnionVectorReq :: WriteVector (WriteUnion a) -> (Field, Field)
writeUnionVectorReq (WriteVector xs) =
  bimap W.vector W.vector $ foldMap writeElem xs
  where
  writeElem :: WriteUnion a -> ([Field], [Field])
  writeElem u =
    case u of
      Some (t, v) -> ([W.inline W.word8 t], [v])
      -- in a vector of unions, a `none` value is encoded as the circular reference 0.
      None        -> ([W.inline W.word8 0], [W.inline W.word32 0])

writeUnionVectorOpt :: Maybe (WriteVector (WriteUnion a)) -> (Field, Field)
writeUnionVectorOpt =
  maybe (W.missing, W.missing) writeUnionVectorReq
