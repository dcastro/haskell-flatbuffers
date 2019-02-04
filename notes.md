# Notes

* when writing to a table, any field can be "missing"
* when reading from a table:
  - a missing field of type table/string/vector/struct will default to `null`/`Nothing` in the host language
  - a missing field of type `bool`/numeric will default to the corresponding default values, which can be configured in the schema.

## Flatbuffers limitations

* Vectors cannot contain:
  * other vectors
  * null/missing elements
* `bool`/numeric types - cannot be null/missing.
  * `bool`/numeric types have default values (configurable), which the user should be able to select.
* structs *can* be null/missing.
  * nested stucts *can't* be null/missing. When a struct contains nested structs, either the whole thing is null, or it isn't/
  * structs don't have default values, but its fields do.
  * structs can only contain: numeric fields, boolean, unions???

## Implementation specific limitations

* Vector of unions / vectors of unions of structs are not supported by all languages (Languages where this is supported: typescript)
* (Java) Cannot distinguish between a missing/null vector and an empty vector (workaround is to wrap vector in table).

## TODO

* Add support for:
  * unions of structs / unions of strings: <https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#unions>
  * (possibly) vectors of unions
* Rules to be enforced at the type level
  * offsets can't be written to structs
  * vectors cannot contain `missing`
  * vector elements must all be of the same type - or at least of the same size
* Override table alignment
* Be able to choose the order in which fields are laid out inside the table, without affecting the order of the vtable. Check out the java classes generated for `table T {c: Color; u: SomeUnion; b: bool;}`.
* "Force defaults" mode
* Generalize code, `[Field]` -> `Traversable f => f Field`
* Bang patterns, unpacked pragma, `$!`
* Use strict version of `Maybe`, etc.
* `FlatBuffers.Read`
  * Parse a flatbuffer from a strict bytestring
  * Support for reading lazy text and maybe strings
  * Failure modes:
    * Non-nullable fields (e.g. numerical, booleans and enums)
      * throw, return default
    * Nullable fields (e.g. strings, structs, tables)
      * throw, return Maybe
  * check if a field is present with `HasField`?
* Define our own `Widen a b` typeclass