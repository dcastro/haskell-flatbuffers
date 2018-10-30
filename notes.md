# Notes

## Flatbuffers limitations

* Cannot distinguish empty vector from absent vector (workaround is to wrap vector in a table).
  * This same problem affects ints (`Just 0` vs `Nothing`), etc.
* Vectors (e.g. of strings) cannot contains nulls, see <https://github.com/google/flatbuffers/issues/4704>
* Vector of unions / vectors of unions of structs are not supported by all languages (supported in typescript)
* Structs, unlike other scalars, are nullable. But when a struct `y` is nested inside a struct `x`, either the whole thing is null, or it isn't. You can't have a nullable nested struct.

## TODO

* Add support for:
  * unions of structs / unions of strings: <https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#unions>
  * (possibly) vectors of unions
* Memory alignment
* Rules to be enforced at the type level
  * offsets can't be written to structs
  * Vectors cannot contain `missing`
* Be able to choose the order in which fields are laid out inside the table, without affecting the order of the vtable. Check out the java classes generated for `table T {c: Color; u: SomeUnion; b: bool;}`.
* "Force defaults" mode
