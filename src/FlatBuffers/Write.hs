{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module FlatBuffers.Write
  ( WriteUnion(..)
  , WriteTable(..)
  , WriteStruct(..)
  , encode
  , encodeWithFileIdentifier
  , encodeWithFileIdentifier'
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
import           FlatBuffers.Internal.Write
import qualified FlatBuffers.Internal.Write as W
import           FlatBuffers.Types

encode :: WriteTable a -> BSL.ByteString
encode (WriteTable table) = root table

encodeWithFileIdentifier :: forall a. HasFileIdentifier a => WriteTable a -> BSL.ByteString
encodeWithFileIdentifier = encodeWithFileIdentifier' (getFileIdentifier @a)

encodeWithFileIdentifier' :: forall a. FileIdentifier -> WriteTable a -> BSL.ByteString
encodeWithFileIdentifier' fi (WriteTable table) = rootWithFileIdentifier fi table

newtype WriteTable a = WriteTable { unWriteTable :: Field }

newtype WriteStruct a = WriteStruct { unWriteStruct :: InlineField }

data WriteUnion a
  = None
  | Some !(Word8, Field)

writeTable :: [Field] -> WriteTable a
writeTable = WriteTable . table

writeStruct :: Alignment -> NonEmpty InlineField -> WriteStruct a
writeStruct structAlign xs = WriteStruct (struct structAlign xs)

writeUnion :: Word8 -> WriteTable a -> WriteUnion b
writeUnion n (WriteTable t) = Some (n, t)

none :: WriteUnion a
none = None

deprecated :: Field
deprecated = missing

optional :: (a -> Field) -> Maybe a -> Field
optional = maybe missing

optionalDef :: Eq a => a -> (a -> Field) -> (Maybe a -> Field)
optionalDef dflt write ma =
  case ma of
    Just a | a /= dflt -> write a
    _                  -> missing

writeVector :: (a -> Field) -> ([a] -> Field)
writeVector write xs =
  vector (write <$> xs)

writeUnionType :: WriteUnion a -> Field
writeUnionType !a =
  case a of
    None        -> missing
    Some (t, _) -> inline word8 t

writeUnionValue :: WriteUnion a -> Field
writeUnionValue !a =
  case a of
    None        -> missing
    Some (_, v) -> v

writeUnionVectorReq :: [WriteUnion a] -> (Field, Field)
writeUnionVectorReq xs =
  bimap vector vector $ foldMap writeElem xs
  where
  writeElem :: WriteUnion a -> ([Field], [Field])
  writeElem u =
    case u of
      Some (t, v) -> ([inline word8 t], [v])
      -- in a vector of unions, a `none` value is encoded as the circular reference 0.
      None        -> ([inline word8 0], [inline word32 0])

writeUnionVectorOpt :: Maybe [WriteUnion a] -> (Field, Field)
writeUnionVectorOpt =
  maybe (missing, missing) writeUnionVectorReq


