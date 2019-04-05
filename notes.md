## Links

* Spec: <https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md>
* Schema grammar: <https://google.github.io/flatbuffers/flatbuffers_grammar.html>
  * The grammar for unions seems to be wrong. It allows `union U { T = 3 }` which shouldn't be possible.
    It also doesn't support aliases like `union U { A: T }`.
* More schema info (attributes, etc): <https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html>

## Notes

* Data types: tables, vectors, string, struct, numeric, bool, union, enums
  * numeric: signed/unsigned 8/16/32/64-bit integers, 32/64-bit floating point.
  * unions are encoded as two contiguous fields, each of which has its own entry in the vtable:
    * the byte: an unsigned byte that signals which member of the union this is
    * the pointer: an offset to where the actual member is located
      * QUESTION: this is true when the member is a table. what if it's a struct or a string?

* when writing to a table, any field can be null/missing
* when writing to a vector, no element can be null/missing
* when reading from a table:
  - a missing field of type table/vector/string/struct will default to `null`/`Nothing` in the host language
  - a missing field of type bool/numeric/enum will default to the corresponding default values, which can be configured in the schema.
  - a missing field of type union should default to some concept of "none". We consider that a union is "missing" if "the byte" field is missing or if it's 0.
    - QUESTION: what to do if the byte field of a union is invalid? fail? default to "none"? leave it to the user?

* Containers
  * Table: can contain all types.
  * Structs: can contain other structs, numeric, bool, enums. Can't contain tables, vectors, strings, unions, or "missing" elements.
  * Vectors: can contain tables, strings, structs, numeric, bool, unions, enums. Can't contain other vectors, or "missing" elements.
    * > A union vector is in reality two separate vectors: a type vector and an offset vector in place of a single unions type and value fields in table. See unions.
    * > each type vector element represents the type of the table in the corresponding value element. If an element is of type NONE the value offset must be stored as 0 which is a circular reference. This is the only offset that can have the value 0.
  * Union: union members can be tables. They cannot be unions themselves.
    * > A later addition (mid 2017) to the format allows for structs and strings to also be member of a union.
    * This is only supported in C++ so far, so I'll wait for wider support before adding it to this project.
    * Note that union members can only be strings if they have an alias: `union U { Name: string}`. `union U { string }` wouldn't compile.

* Structs
  * Structs can be null when embedded in tables, but not when embedded in other structs.
    When a struct contains nested structs, either the whole thing is null, or it isn't.

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


## Implementation specific limitations

* Vector of unions / vectors of unions of structs are not supported by all languages (Languages where this is supported: typescript)
* (Java) Cannot distinguish between a missing/null vector and an empty vector (workaround is to wrap vector in table).

## TODO

* Code generation:
  - give the user the option to generate code for imported types (like flatc's --gen-all)

* Guard against writing buffers longer than 2^31-1 bytes 
    * this should (implicitly) also protect against strings/vectors with a length that wouldn't fit in a uoffset (word32) field.
* Add support for:
  * unions of structs / unions of strings: <https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#unions>
  * `file_identifier` and `root_type`
  * "size prefix before the standard header"?
  * `nested_flatbuffer` attribute
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

## Differences between `flatc` and `haskell-flatbuffers`

* In `flatc`, attributes must be declared before usage.
  Also, structs can only refer to previously declared structs/enums, not to stucts/enums declared later on in the file.
  In `haskell-flatbuffers`, the order in which they are declared doesn't matter.


  [scalars]: https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#scalars
  [empty structs]: https://github.com/google/flatbuffers/commit/160e8f2fdc9d5989e652709fae3fac0bd9aaed14