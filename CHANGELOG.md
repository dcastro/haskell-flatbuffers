# Changelog


## 0.2.0.0


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
