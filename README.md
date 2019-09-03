# Haskell Flatbuffers

[![Build Status](https://travis-ci.com/dcastro/haskell-flatbuffers.svg?branch=master)](https://travis-ci.com/dcastro/haskell-flatbuffers)

WIP. An implementation of the flatbuffers protocol in Haskell.


## Not yet implemented

### Features

- [ ] gRPC
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

- [ ] TH: sort table fields by size + support `original_order` attribute.
- [ ] Add support for storing unboxed vectors, which do not have a `Foldable` instance. Maybe use `MonoFoldable` from the `mono-traversable` package.
- [ ] Improve error messages during `SemanticAnalysis` stage, provide source code position.

