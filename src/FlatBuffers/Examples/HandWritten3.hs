module FlatBuffers.Examples.HandWritten3 where

import Data.Int
import Data.Text (Text)
import FlatBuffers.Internal.Write3 as W3 hiding (Person)

data Person

person :: Maybe Int32 -> Maybe (Location Text) -> Maybe (Location [Text]) -> Write (Location Person)
person age name friends = do
  W3.writeTable @Person 3 $ mconcat
    [
      optional W3.writeInt32TableField 0 age
      ,
      optional W3.writeOffsetTableField 1 name
      ,
      optional W3.writeOffsetTableField 2 friends
    ]

data People

people :: Maybe (Location [Person]) -> Write (Location People)
people vector = do
  W3.writeTable @People 1 $
    optional W3.writeOffsetTableField 0 vector
