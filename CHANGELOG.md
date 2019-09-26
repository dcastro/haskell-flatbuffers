# Changelog

## v0.2.0.0

* Read vector length upfront, rather than on every access.
  * `FlatBuffers.Vector.length` changed from `Either ReadError Int32` to `Int32`.
* Added:
  * `FlatBuffers.Vector.take`
  * `FlatBuffers.Vector.drop`


