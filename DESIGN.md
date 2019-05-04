## Design decisions

* A buffer should never be larger than [2^31 - 1 bytes][size limits], but these limits are not checked by the library.
  We assume e.g. strings and vectors provided by the user do not exceed these limits.
  Exceeding them might result in overflows.



[size limits]: https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#size-limits
