## Design decisions


### Buffer size

A buffer should never be larger than [2^31 - 1 bytes][size limits], but these limits are not checked by the library.
We assume e.g. strings and vectors provided by the user do not exceed these limits.
Exceeding them might result in overflows.


### Imports - scope

Juts like in the official implementation, when a schema is imported, it becomes available to all other imported schemas.
For example: Schema X imports schemas Y and Z. Schema Y now has access to all definitions declared in Schema Z and vice-versa.

### Imports - relative paths

The official implementation assumes relative paths in `include` statements are relative to the directory of the *root* schema.

We, on other other hand, assume paths are relative to the directory of the *current* schema.

For example, using the official implementation:


|      File name      |         Content          |
| ------------------- | ------------------------ |
| `schemas/a.fbs`     | `include "./sub/b.fbs";` |
| `schemas/sub/b.fbs` | `include "./sub/c.fbs";` |
| `schemas/sub/c.fbs` | `table T {}`             |

Using this implementation:

|      File name      |         Content          |
| ------------------- | ------------------------ |
| `schemas/a.fbs`     | `include "./sub/b.fbs";` |
| `schemas/sub/b.fbs` | `include "./c.fbs";`     |
| `schemas/sub/c.fbs` | `table T {}`             |

[size limits]: https://github.com/dvidelabs/flatcc/blob/master/doc/binary-format.md#size-limits
