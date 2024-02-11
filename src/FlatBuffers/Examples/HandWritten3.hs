module FlatBuffers.Examples.HandWritten3 where

import Data.Coerce (coerce)
import Data.Int
import Data.Text (Text)
import Data.Word
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


----------------------------------------------------------------------------
-- Unions
----------------------------------------------------------------------------

data Sword
sword :: Maybe (Location Text) -> Write (Location Sword)
sword x = W3.writeTable @Sword 1 $ optional W3.writeOffsetTableField 0 x

data Axe
axe :: Maybe Int32 -> Write (Location Axe)
axe x = W3.writeTable @Axe 1 $ optional W3.writeInt32TableField 0 x

data Weapon

weaponSword :: Location Sword -> UnionLocation Weapon
weaponSword = UnionLocation 1 . coerce

weaponAxe :: Location Axe -> UnionLocation Weapon
weaponAxe = UnionLocation 2 . coerce

-- Table with 1 union field
data Character

character :: Maybe (UnionLocation Weapon) -> Write (Location Character)
character weapon =
  W3.writeTable @Character 2 $ mconcat
    [
      optional W3.writeOffsetTableField 0 (ulLocation <$> weapon)
      ,
      optional W3.writeWord8TableField 0 (getUnionType . ulType <$> weapon)
    ]

-- Table with a union vector field
data Weapons

weapons :: Maybe (Location [Weapon], Location [UnionType Weapon]) -> Write (Location Weapons)
weapons ws =
  W3.writeTable @Weapons 2 $ mconcat
    [
      optional W3.writeOffsetTableField 1 (fst <$> ws)
      ,
      optional W3.writeOffsetTableField 1 (snd <$> ws)
    ]
