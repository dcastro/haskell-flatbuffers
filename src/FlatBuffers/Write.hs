{-# LANGUAGE TypeApplications #-}

module FlatBuffers.Write where

import           Data.Bifunctor       (bimap)
import qualified Data.ByteString.Lazy as BSL
import           Data.Tagged          (Tagged (..), untag)
import           Data.Word            (Word8)
import           FlatBuffers          (Field, InlineField)
import qualified FlatBuffers          as F

newtype UnionField =
  UnionField (Maybe (Word8, Field))

encode :: Tagged t Field -> BSL.ByteString
encode = F.root . untag

mb :: (a -> Field) -> Maybe a -> Field
mb = maybe F.missing

mb2 :: (a -> (Field, Field)) -> Maybe a -> (Field, Field)
mb2 = maybe (F.missing, F.missing)

unionTypeField :: Tagged a UnionField -> Field
unionTypeField (Tagged (UnionField (Just (t, _)))) = F.scalar F.word8 t
unionTypeField _                                   = F.scalar F.word8 0

unionValueField :: Tagged a UnionField -> Field
unionValueField (Tagged (UnionField (Just (_, v)))) = v
unionValueField _                                   = F.missing

vector :: (a -> Field) -> [a] -> Field
vector f xs = F.vector @[] $ fmap f xs

none :: Tagged a UnionField
none = Tagged (UnionField Nothing)

unionVector :: [Tagged a UnionField] -> (Field, Field)
unionVector xs = bimap F.vector F.vector (foldMap unionVecElem xs)
  where
    unionVecElem :: Tagged a UnionField -> ([Field], [Field])
    unionVecElem (Tagged (UnionField uf)) =
      case uf of
        Just (t, v) -> ([F.scalar F.word8 t], [v])
        Nothing     -> ([F.scalar F.word8 0], [F.scalar F.word32 0])
