# Haskell Flatbuffers

[![Build Status](https://travis-ci.com/dcastro/haskell-flatbuffers.svg?branch=master)](https://travis-ci.com/dcastro/haskell-flatbuffers)

<!-- TOC depthFrom:2 updateOnSave:false -->

- [Getting started](#getting-started)
  - [Vectors](#vectors)
  - [Enums](#enums)
  - [Structs](#structs)
  - [Unions](#unions)
- [Codegen](#codegen)
  - [Codegen Examples](#codegen-examples)
- [TODO](#todo)
  - [Features](#features)
  - [Other](#other)

<!-- /TOC -->

An implementation of the [flatbuffers protocol][flatbuffers] in Haskell.

## Getting started

1. Start off by writing a [flatbuffers schema][schema] with the data structures you want to serialize/deserialize.
    ```cpp
    namespace Data.Game;

    table Monster {
      name: string;
      hp: int (required);
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
4. In this simple example, the following declarations will be generated for you.
    ```haskell
    data Monster

    -- Constructor
    monster :: Maybe Text -> Int32 -> WriteVector Text -> WriteTable Monster

    -- Accessors
    monsterName      :: Table Monster -> Either ReadError (Maybe Text)
    monsterHp        :: Table Monster -> Either ReadError Int32
    monsterLocations :: Table Monster -> Either ReadError (Vector Text)
    ```

<!-- In this simple example, `mkFlatBuffers` will generate the following declarations: -->

Before using these, go ahead and add these imports/extensions to your module:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Data.Text          ( Text )
import           FlatBuffers
import qualified FlatBuffers.Vector as Vector
```

We can now construct a flatbuffer using `encode` and read it using `decode`:

```haskell
-- Writing
let byteString = encode $
      monster
        (Just "Poring")
        50
        (Vector.fromList 2 ["Prontera Field", "Payon Forest"])

-- Reading
do
  someMonster <- decode byteString
  name        <- monsterName someMonster
  hp          <- monsterHp someMonster
  locations   <- monsterLocations someMonster >>= Vector.toList
  Right ("Monster: " <> show name <> " (" <> show hp <> " HP) can be found in " <> show locations)
```

### Vectors

To work with vectors, you should import `FlatBuffers.Vector` (preferably qualified).

The most generic way of creating a vector is with `fromFoldable`.
Check out the module's docs for other more specialized functions.

```haskell
fromFoldable ::
     Foldable f
  => Int32      -- ^ n: the number of elements in xs
  -> f a        -- ^ xs: a collection
  -> WriteVector a
```

### Enums

Given the following enum declaration:

```cpp
enum Color: short { Red, Green, Blue }
```

The following code will be generated:

```haskell
data Color
  = ColorRed
  | ColorGreen
  | ColorBlue
  deriving (Eq, Show, Read, Ord, Bounded)

toColor   :: Int16 -> Maybe Color
fromColor :: Color -> Int16
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

-- Writing
let byteString = encode $
      monster (Just (fromColor ColorBlue))

-- Reading
do
  someMonster <- decode byteString
  short       <- monsterColor someMonster
  case toColor short of
    Just ColorRed   -> "This monster is red"
    Just ColorGreen -> "This monster is green"
    Just ColorBlue  -> "This monster is blue"
    Nothing         -> "Unknown color: " <> show short -- Forwards compatibility
```

### Structs

Given the following struct declaration:

```cpp
struct Coord {
  x: long;
  y: long;
}
```

The following code will be generated:

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

```cpp
table Monster {
  position: Coord (required);
}
```

```haskell
data Monster

monster         :: WriteStruct Coord -> WriteTable Monster
monsterPosition :: Table Monster -> Either ReadError (Struct Coord)

-- Writing
let byteString = encode $
      monster (coord 123 456)

-- Reading
do
  someMonster <- decode byteString
  pos         <- monsterPosition someMonster
  x           <- coordX pos
  y           <- coordY pos
  Right ("Monster is located at " <> show x <> ", " <> show y)
```

### Unions

Given the following union declaration:

```cpp
table Sword { power: int (required); }
table Axe { power: int (required); }
union Weapon { Sword, Axe }
```

The following code will be generated:

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

```cpp
table Character {
  weapon: Weapon;
}
```

```haskell
data Character

character       :: WriteUnion Weapon -> WriteTable Character
characterWeapon :: Table Character -> Either ReadError (Union Weapon)

-- Writing
let byteString = encode $
      character
        (weaponSword (sword 1000))

-- Reading
do
  someCharacter <- decode byteString
  weapon        <- characterWeapon someCharacter
  case weapon of
    Union (WeaponSword sword) -> do
      power <- swordPower sword
      Right ("Weilding a sword with " <> show power <> " Power.")
    Union (WeaponAxe axe) -> do
      power <- axePower axe
      Right ("Weilding an axe with " <> show power <> " Power.")
    UnioneNone        -> Right "Character has no weapon"
    UnionUnknown byte -> Left "Unknown weapon" -- Forwards compatibility
```

Note that unions are *always* optional. Adding the `required` attribute to an union field has no effect.

To create a character with no weapon, use `none :: WriteUnion a`

```haskell
let byteString = encode $
      character none
```

For more info on code generation and examples, see [codegen](#codegen).

## Codegen

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

### Codegen Examples

There are lots of examples in the [test/Examples][examples] folder and the [`THSpec`][thspec] module.

In particular, `test/Examples/schema.fbs` and `test/Examples/vector_of_unions.fbs` contain a variety of data structures and `Examples.HandWritten` demonstrates what the code generated by `mkFlatBuffers` would look like.


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
- [ ] `bit_flags` attribute
- [ ] `hash` attribute
    - [Docs](https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html)
    - [Docs](https://google.github.io/flatbuffers/flatbuffers_guide_use_cpp.html#flatbuffers_cpp_object_based_api)
- [ ] DSL that allows sharing of data (e.g. reuse an offset to a string/table)
- [ ] `shared` attribute
    - [Docs](https://google.github.io/flatbuffers/flatbuffers_guide_use_cpp.html#flatbuffers_cpp_object_based_api)

### Other

- [ ] TH: sort table fields by size + support `original_order` attribute
- [ ] Add support for storing unboxed vectors, which do not have a `Foldable` instance. Maybe use `MonoFoldable` from the `mono-traversable` package
- [ ] Enrich `Vector` API: drop, take, null, folds, sum, elem, for_, traverse_, ideally support most of operations in `Data.Foldable`
- [ ] Add `MonoFoldable (Vector a)` instance
- [ ] Improve error messages during `SemanticAnalysis` stage, provide source code location
- [ ] Try alternative bytestring builders: `fast-builder`, `blaze-builder`
- [ ] Try alternative bytestring parsers: `cereal`
- [ ] Better support for enums

 [flatbuffers]: https://google.github.io/flatbuffers/
 [schema]: https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html
 [examples]: https://github.com/dcastro/haskell-flatbuffers/tree/master/test/Examples
 [thspec]: https://github.com/dcastro/haskell-flatbuffers/blob/master/test/FlatBuffers/Internal/Compiler/THSpec.hs
