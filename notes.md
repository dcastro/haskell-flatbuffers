# Notes


* Data types: tables, vectors, string, struct, numeric, bool, union, enums
  * numeric: signed/unsigned 8/16/32/64-bit integers, 32/64-bit floating point.
  * unions are encoded as two contiguous fields:
    * the byte: an unsigned byte that signals which member of the union this is
    * the pointer: an offset to where the actual member is located
      * QUESTION: this is true when the member is a table. what if it's a struct? or an int32? or another union?

* when writing to a table, any field can be null/missing
* when writing to a vector, no element can be null/missing
* when reading from a table:
  - a missing field of type table/vector/string/struct will default to `null`/`Nothing` in the host language
  - a missing field of type bool/numeric/enum will default to the corresponding default values, which can be configured in the schema.
  - a missing field of type union should default to some concept of "none". We consider that a union is "missing" if "the byte" field is missing or if it's 0.
    - QUESTION: what to do if the byte field of a union is invalid? fail? default to "none"? leave it to the user?

* vectors of unions?
  > A vector can also hold unions, but it is not supported by all implementations. A union vector is in reality two separate vectors: a type vector and an offset vector in place of a single unions type and value fields in table. See unions.

* Enum
  * values must start at 0 and be declared in ascending order
  * underlying type is integral
  * [the spec says][scalars] an enum with an underlying type of bool is technically possible, but this leads to all sorts of nonsense, e.g. `enum Geg: bool { Qw, We = 1, Ui = 2}`. Furthermore, in Java, an enum with bool leads to generated code that doesn't compile

* Recursiveness
  * Tables can reference themselves
  * The C++ code generator allows recursive structs, but the Java and TypeScript generators hang. Either way, the spec disallows this: `A struct cannot contain fields that contain itself directly or indirectly`.

* Emptiness
  * Tables can be empty (i.e. have 0 fields).
  * Structs cannot be empty, as of [2018-10-30][empty structs].

## Flatbuffers limitations

* Vectors cannot contain:
  * other vectors
  * null/missing elements
* `bool`/numeric types - cannot be null/missing.
  * `bool`/numeric types have default values (configurable), which the user should be able to select.
* structs *can* be null/missing.
  * nested stucts *can't* be null/missing. When a struct contains nested structs, either the whole thing is null, or it isn't.
  * structs don't have default values, but its fields do.
  * structs can only contain: numeric fields, boolean, unions???

## Implementation specific limitations

* Vector of unions / vectors of unions of structs are not supported by all languages (Languages where this is supported: typescript)
* (Java) Cannot distinguish between a missing/null vector and an empty vector (workaround is to wrap vector in table).

## TODO

* Add support for:
  * unions of structs / unions of strings: <https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#unions>
  * (possibly) vectors of unions
  * `file_identifier` and `root_type`
  * "size prefix before the standard header"?
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



  [scalars]: https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#scalars
  [empty structs]: https://github.com/google/flatbuffers/commit/160e8f2fdc9d5989e652709fae3fac0bd9aaed14