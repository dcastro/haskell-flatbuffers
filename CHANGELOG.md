# Changelog

## 0.4.0.0 (2024-02-06)

* Added support for `text` >= 2.0 and `template-haskell` >= 2.18.0.0, removed support for older versions.
* Tested with GHC 9.2.5, 9.4.8 and 9.6.4.
* Forbid writing/reading `Nothing` to/from union vectors and required unions fields.

## 0.3.0.0 (2020-11-14)

* `FlatBuffers.Vector.toByteString` renamed to `FlatBuffers.Vector.toLazyByteString`
* Allow trailing comma after the last enum/union field for compatibility with flatc.
* If a Haskell keyword is used as the name of a table or struct, then suffix the name of the generated constructor with an underscore.

## 0.2.0.0 (2019-10-21)


* Add support for bitmasks, i.e. enums with the `bit_flags` attribute.
* `FlatBuffers.Vector.length` changed from `Either ReadError Int32` to `Int32`.
  * Vector length is now read once upfront, rather than on every access.
* Added to `FlatBuffers.Vector`:
  * `fromByteString`
  * `fromLazyByteString`
  * `fromMonoFoldable` (supports `Data.Vector.Unboxed` and `Data.Vector.Storable`)
  * `take`
  * `drop`
  * `toByteString`
* TemplateHaskell:
  * Added `colorName` for enums.
  * Fixed error messages when running `ghcid` (they used to be truncated).


## 0.1.0.0 (2019-09-22)

* First version.
