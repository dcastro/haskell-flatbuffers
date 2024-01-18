# Haskell Flatbuffers

An implementation of the [flatbuffers protocol][flatbuffers] in Haskell.

[![Build Status](https://travis-ci.com/dcastro/haskell-flatbuffers.svg?branch=master)](https://travis-ci.com/dcastro/haskell-flatbuffers)
[![Hackage](https://img.shields.io/hackage/v/flatbuffers)](http://hackage.haskell.org/package/flatbuffers)

- [Getting started](#getting-started)
  - [Codegen](#codegen)
  - [Enums](#enums)
  - [Bit flags / Bitmasks](#bit-flags--bitmasks)
  - [Structs](#structs)
  - [Unions](#unions)
  - [File Identifiers](#file-identifiers)
- [TODO](#todo)


## Getting started

1. Start off by writing a [flatbuffers schema][schema] with the data structures you want to serialize/deserialize.
    ```
    namespace Data.Game;

    table Monster {
      name: string;
      hp: int;
      locations: [string] (required);
    }
    ```
2. Create a Haskell module named after the namespace in the schema.
    ```haskell
    module Data.Game where
    ```
3. Use `mkFlatBuffers` to generate constructors and accessors for the data types in your schema.
    ```haskell
    {-# LANGUAGE TemplateHaskell #-}

    module Data.Game where
    import FlatBuffers

    $(mkFlatBuffers "schemas/game.fbs" defaultOptions)
    ```
4. The following declarations will be generated for you.
    ```haskell
    data Monster

    -- Constructor
    monster :: Maybe Text -> Maybe Int32 -> WriteVector Text -> WriteTable Monster

    -- Accessors
    monsterName      :: Table Monster -> Either ReadError (Maybe Text)
    monsterHp        :: Table Monster -> Either ReadError Int32
    monsterLocations :: Table Monster -> Either ReadError (Vector Text)
    ```

We can now construct a flatbuffer using `encode` and read it using `decode`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Lazy (ByteString)
import           FlatBuffers
import qualified FlatBuffers.Vector as Vector

-- Writing
byteString = encode $
      monster
        (Just "Poring")
        (Just 50)
        (Vector.fromList 2 ["Prontera Field", "Payon Forest"])

-- Reading
readMonster :: ByteString -> Either ReadError String
readMonster byteString = do
  someMonster <- decode byteString
  name        <- monsterName someMonster
  hp          <- monsterHp someMonster
  locations   <- monsterLocations someMonster >>= Vector.toList
  Right ("Monster: " <> show name <> " (" <> show hp <> " HP) can be found in " <> show locations)
```

For the rest of this document, we'll assume these imports/extensions are enabled:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           FlatBuffers
import qualified FlatBuffers.Vector as Vector
```

### Codegen

You can check exactly which declarations were generated by browsing your module in ghci:

```plain
λ> :m Data.Game FlatBuffers FlatBuffers.Vector
λ> :browse Data.Game
data Monster
monster :: Maybe Int32 -> WriteTable Monster
monsterHp :: Table Monster -> Either ReadError Int32
```

Or by launching a local hoogle server with Stack:

```plain
> stack hoogle --rebuild --server
```

There are lots of examples in the [test/Examples][examples] folder and the [`THSpec`][thspec] module.

In particular, `test/Examples/schema.fbs` contains a variety of data structures and `Examples.HandWritten` demonstrates what the code generated by `mkFlatBuffers` would look like.

### Enums

```
enum Color: short {
  Red, Green, Blue
}
```

Given the enum declarationa above, the following code will be generated:

```haskell
data Color
  = ColorRed
  | ColorGreen
  | ColorBlue
  deriving (Eq, Show, Read, Ord, Bounded)

toColor   :: Int16 -> Maybe Color
fromColor :: Color -> Int16

colorName :: Color -> Text
```

Usage:

```
table Monster {
  color: Color;
}
```

```haskell
data Monster

monster      :: Maybe Int16 -> WriteTable Monster
monsterColor :: Table Monster -> Either ReadError Int16
```

```haskell
-- Writing
byteString = encode $
      monster (Just (fromColor ColorBlue))

-- Reading
readMonster :: ByteString -> Either ReadError Text
readMonster byteString = do
  someMonster <- decode byteString
  i           <- monsterColor someMonster
  case toColor i of
    Just color -> Right ("This monster is " <> colorName color)
    Nothing    -> Left ("Unknown color: " <> show i) -- Forwards compatibility
```

### Bit flags / Bitmasks

```
enum Colors: uint16 (bit_flags) {
  Red, Green, Blue
}
```

Given the enum declarationa above, the following code will be generated:

```haskell
colorsRed, colorsGreen, colorsBlue :: Word16
colorsRed = 1
colorsGreen = 2
colorsBlue = 4

allColors :: [Word16]

colorsNames :: Word16 -> [Text]
```

Usage:

```
table Monster {
  colors: Colors = "Red Blue";
}
```

```haskell
data Monster

monster       :: Maybe Word16 -> WriteTable Monster
monsterColors :: Table Monster -> Either ReadError Word16
```

```haskell
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO)
import Data.Bits ((.|.), (.&.))
import qualified Data.Text.IO as Text

-- Writing
byteString = encode $
      monster (Just (colorsBlue .|. colorsGreen))

-- Reading
readMonster :: (MonadIO m, MonadError ReadError m) => ByteString -> m ()
readMonster byteString = do
  someMonster <- liftEither $ decode byteString
  colors      <- liftEither $ monsterColors someMonster

  let isRed = colors .&. colorsRed /= 0
  liftIO $ putStrLn $ "Is this monster red? " <> if isRed then "Yes" else "No"

  liftIO $ Text.putStrLn $ "Monster colors: " <> Text.intercalate ", " (colorsNames colors)
```


### Structs

```
struct Coord {
  x: long;
  y: long;
}
```

Given the struct declaration above, the following code will be generated:

```haskell
data Coord
instance IsStruct Coord

--  Constructor
coord :: Int64 -> Int64 -> WriteStruct Coord

-- Accessors
coordX :: Struct Coord -> Either ReadError Int64
coordY :: Struct Coord -> Either ReadError Int64
```

Usage:

```
table Monster {
  position: Coord (required);
}
```

```haskell
data Monster

monster         :: WriteStruct Coord -> WriteTable Monster
monsterPosition :: Table Monster -> Either ReadError (Struct Coord)
```

```haskell
-- Writing
byteString = encode $
      monster (coord 123 456)

-- Reading
readMonster :: ByteString -> Either ReadError String
readMonster byteString = do
  someMonster <- decode byteString
  pos         <- monsterPosition someMonster
  x           <- coordX pos
  y           <- coordY pos
  Right ("Monster is located at " <> show x <> ", " <> show y)
```

### Unions

```
table Sword { power: int; }
table Axe { power: int; }
union Weapon { Sword, Axe }
```

Given the union declaration above, the following code will be generated:

```haskell
-- Accessors
data Weapon
  = WeaponSword !(Table Sword)
  | WeaponAxe   !(Table Axe)

-- Constructors
weaponSword :: WriteTable Sword -> WriteUnion Weapon
weaponAxe   :: WriteTable Axe   -> WriteUnion Weapon
```

Usage:

```
table Character {
  weapon: Weapon;
}
```

```haskell
data Character

character       :: WriteUnion Weapon -> WriteTable Character
characterWeapon :: Table Character -> Either ReadError (Union Weapon)
```

```haskell
-- Writing
byteString = encode $
      character
        (weaponSword (sword (Just 1000)))

-- Reading
readCharacter :: ByteString -> Either ReadError String
readCharacter byteString = do
  someCharacter <- decode byteString
  weapon        <- characterWeapon someCharacter
  case weapon of
    Union (WeaponSword sword) -> do
      power <- swordPower sword
      Right ("Weilding a sword with " <> show power <> " Power.")
    Union (WeaponAxe axe) -> do
      power <- axePower axe
      Right ("Weilding an axe with " <> show power <> " Power.")
    UnionNone         -> Right "Character has no weapon"
    UnionUnknown byte -> Left "Unknown weapon" -- Forwards compatibility
```

Note that, like in the official FlatBuffers implementation, unions are *always* optional.
Adding the `required` attribute to a union field has no effect.

To create a character with no weapon, use `none :: WriteUnion a`

```haskell
byteString = encode $
      character none
```


### File Identifiers

From ["File identification and extension"][schema]:

> Typically, a FlatBuffer binary buffer is not self-describing, i.e. it needs you to know its schema to parse it correctly. But if you want to use a FlatBuffer as a file format, it would be convenient to be able to have a "magic number" in there, like most file formats have, to be able to do a sanity check to see if you're reading the kind of file you're expecting.
>
> Now, you can always prefix a FlatBuffer with your own file header, but FlatBuffers has a built-in way to add an identifier to a FlatBuffer that takes up minimal space, and keeps the buffer compatible with buffers that don't have such an identifier.

```
table Monster { name: string; }

root_type Monster;
file_identifier "MONS";
```

```haskell
data Monster
instance HasFileIdentifier Monster

-- Usual constructor and accessors...
```

We can now construct a flatbuffer using `encodeWithFileIdentifier` and use `checkFileIdentifier` to check if it's safe to decode it to a specific type:

```haskell
{-# LANGUAGE TypeApplications #-}

-- Writing
byteString = encodeWithFileIdentifier $
      monster (Just "Poring")

-- Reading
readName :: ByteString -> Either ReadError (Maybe Text)
readName byteString = do
  if checkFileIdentifier @Monster byteString then do
    someMonster <- decode byteString
    monsterName someMonster
  else if checkFileIdentifier @Character byteString then do
    someCharacter <- decode byteString
    characterName someCharacter
  else
    Left "Unexpected flatbuffer identifier"
```

## TODO

### Features

- [ ] gRPC support
- [ ] Size-prefixed buffers (needed for streaming multiple messages)
    - [flatbuffers/3898](https://github.com/google/flatbuffers/issues/3898)
    - [FlatCC](https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#nested-flatbuffers)
- [ ] Fixed length arrays in structs
    - [flatbuffers/63](https://github.com/google/flatbuffers/issues/63)
    - [flatbuffers/3987](https://github.com/google/flatbuffers/pull/3987)
    - [flatbuffers/5313](https://github.com/google/flatbuffers/pull/5313)
    - [FlatCC](https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#fixed-length-arrays)
- [ ] unions of strings / structs
    - [FlatCC](https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#unions)
- [ ] `key` attribute (See ["Storing dictionaries in a FlatBuffer" section](https://google.github.io/flatbuffers/flatbuffers_guide_use_java_c-sharp.html))
- [ ] `nested_flatbuffer` attribute
- [ ] `hash` attribute
    - [Docs](https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html)
    - [Docs](https://google.github.io/flatbuffers/flatbuffers_guide_use_cpp.html#flatbuffers_cpp_object_based_api)
- [ ] DSL that allows sharing of data (e.g. reuse an offset to a string/table)
- [ ] `shared` attribute
    - [Docs](https://google.github.io/flatbuffers/flatbuffers_guide_use_cpp.html#flatbuffers_cpp_object_based_api)
- [ ] Attach [Haddock documentation to the generated code](https://hackage.haskell.org/package/template-haskell-2.21.0.0/docs/Language-Haskell-TH-Lib.html#g:32).

### Other

- [ ] TH: sort table fields by size + support `original_order` attribute
- [ ] Enrich `Vector` API: drop, take, null, folds, sum, elem, for_, traverse_, ideally support most of operations in `Data.Foldable`
- [ ] Improve error messages during `SemanticAnalysis` stage, provide source code location
- [ ] Try alternative bytestring builders: `fast-builder`, `blaze-builder`
- [ ] Try alternative bytestring parsers: `cereal`

 [flatbuffers]: https://google.github.io/flatbuffers/
 [schema]: https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html
 [examples]: https://github.com/dcastro/haskell-flatbuffers/tree/master/test/Examples
 [thspec]: https://github.com/dcastro/haskell-flatbuffers/blob/master/test/FlatBuffers/Internal/Compiler/THSpec.hs
