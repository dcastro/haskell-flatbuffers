{- HLINT ignore "Avoid lambda" -}

module Compare where

import Criterion
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import FlatBuffers.Internal.Write qualified as W1
import FlatBuffers.Internal.Write2 qualified as W2
import FlatBuffers.Internal.Write3 qualified as W3

data Person = Person
  { personName :: Text
  , personAge :: Int32
  , personFriends :: V.Vector Text
  }

-- TODO: use different strings for names
mkPeople :: Int32 -> Int32 -> V.Vector Person
mkPeople peopleCount friendsCount =
  [1..peopleCount]
  <&> do \i -> Person "abcdefghijk" i $ mkFriends friendsCount
  & V.fromList

mkFriends :: Int32 -> V.Vector Text
mkFriends friendsCount =
  [1..friendsCount]
  <&> do \i -> T.pack $ show i
  & V.fromList

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
              ,
              W1.writeVectorTextTableField $ W1.fromMonoFoldable' p.personFriends
            ]
      ]

write2 :: V.Vector Person -> BS.ByteString
write2 people =
  W2.encode W2.defaultWriteSettings do

    peopleTables <- W2.writeMany people \person -> do
      name <- W2.writeText person.personName
      friends <- W2.fromFoldable =<< W2.writeMany person.personFriends W2.writeText
      W2.writeTable @Person 3 $ mconcat
        [
          W2.writeInt32TableField 0 person.personAge
          ,
          W2.writeOffsetTableField 1 name
          ,
          W2.writeOffsetTableField 2 friends
        ]

    peopleVector <- W2.fromFoldable peopleTables

    W2.writeTable 1 $ W2.writeOffsetTableField 0 peopleVector

write3 :: V.Vector Person -> BS.ByteString
write3 people =
  W3.encode W3.defaultWriteSettings do

    peopleTables <- W3.writeMany people \person -> do
      name <- W3.writeText person.personName
      friends <- W3.fromFoldable =<< W3.writeMany person.personFriends W3.writeText
      W3.writeTable @Person 3 $ mconcat
        [
          W3.writeInt32TableField 0 person.personAge
          ,
          W3.writeOffsetTableField 1 name
          ,
          W3.writeOffsetTableField 2 friends
        ]


    peopleVector <- W3.fromFoldable peopleTables

    W3.writeTable 1 $ W3.writeOffsetTableField 0 peopleVector

groups :: [Benchmark]
groups =
  [ bgroup "compare"
    [ bench "Write1" $ nf write1 $ mkPeople 1000 10000
    , bench "Write2" $ nf write2 $ mkPeople 1000 10000
    , bench "Write3" $ nf write3 $ mkPeople 1000 10000
    ]
  ]
