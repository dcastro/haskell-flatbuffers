{- HLINT ignore "Avoid lambda" -}

module Compare where

import Criterion
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
import Data.Int
import Data.Text (Text)
import Data.Vector qualified as V
import FlatBuffers.Internal.Write qualified as W1
import FlatBuffers.Internal.Write2 qualified as W2
import FlatBuffers.Internal.Write3 qualified as W3

data Person = Person
  { personName :: Text
  , personAge :: Int32
  }

-- TODO: use different strings for names
mkPeopleList :: Int32 -> [Person]
mkPeopleList n = (\i -> Person "abcdefghijk" i) <$> [1..n]

mkPeople :: Int32 -> V.Vector Person
mkPeople = V.fromList . mkPeopleList

write1 :: V.Vector Person -> BSL.ByteString
write1 people =
  W1.encode do
    W1.writeTable
      [ W1.writeVectorTableTableField $ W1.fromMonoFoldable' $ people <&> \p ->
          W1.writeTable
            [
              W1.writeInt32TableField p.personAge
              ,
              W1.writeTextTableField p.personName
            ]
      ]

write2 :: V.Vector Person -> BS.ByteString
write2 people =
  W2.encode W2.defaultWriteSettings do

    peopleTables <- W2.writeMany people \person -> do
      name <- W2.writeText person.personName
      W2.writeTable @Person 2 $ mconcat
        [
          W2.writeInt32TableField 0 person.personAge
          ,
          W2.writeOffsetTableField 1 name
        ]

    peopleVector <- W2.fromFoldable peopleTables

    W2.writeTable 1 $ W2.writeOffsetTableField 0 peopleVector

write3 :: V.Vector Person -> BS.ByteString
write3 people =
  W3.encode W3.defaultWriteSettings do

    peopleTables <- W3.writeMany people \person -> do
      name <- W3.writeText person.personName
      W3.writeTable @Person 2 $ mconcat
        [
          W3.writeInt32TableField 0 person.personAge
          ,
          W3.writeOffsetTableField 1 name
        ]

    peopleVector <- W3.fromFoldable peopleTables

    W3.writeTable 1 $ W3.writeOffsetTableField 0 peopleVector

groups :: [Benchmark]
groups =
  [ bgroup "compare"
    [ bench "Write1" $ nf write1 $ mkPeople 100000
    , bench "Write2" $ nf write2 $ mkPeople 100000
    , bench "Write3" $ nf write3 $ mkPeople 100000
    ]
  ]
