# Runtime memory

The stack has a fixed size of 64 KiB and stores pointers to heap data.
The runtime gets all its memory with anonymous private `mmap`, not with `brk`.
It does not give memory back to the operating system. When `mmap` fails, the
program stops with `out of memory`.

The heap holds two kinds of memory:

- Scalar cells. Each `int`, `bool` or `ptr` value that the runtime computes
  goes into an 8 byte cell. Cells are in 64 KiB chunks. Each chunk is one
  mapping. Cells have no header and you cannot free them.
- Blocks. Each `ptr.alloc` makes one block. An 8 byte header comes before the
  returned pointer. The header holds the block size, with the header, and bit
  0 is set while the block is in use. A block has at least 8 bytes of memory.
  Blocks are in regions. A region is one mapping of at least 1 MiB with a 16
  byte header. When a block does not fit in the current region, the rest of
  that region becomes a free block and a new region is mapped.

Cells and block pointers are 8 byte aligned. `ptr.free` puts a block in a free
list that is sorted by address and merges adjacent free blocks of the same
region. Blocks of different regions never merge. `ptr.alloc`
uses the first free block that is large enough. It splits off the rest when at
least 16 bytes remain. Memory from `ptr.alloc` is always zero.

These errors write a message to stderr and exit with status 1:

- `Runtime error: negative allocation size`: `ptr.alloc` got a negative size.
- `Runtime error: out of memory`: the heap cannot grow.
- `Runtime error: invalid free` and `Runtime error: double free`: `ptr.free`
  found a problem.

## Freeing memory

Use the exact pointer that `ptr.alloc` returned. `ptr.null` does nothing.
The free is shallow: memory that the block points to stays allocated. After
freeing, all copies of the pointer and all pointers into the block are
dangling. You must not use them.

Nothing frees memory automatically. The standard library frees only memory
that it owns:

- `src old new ptr.realloc` allocates `new` bytes, copies the first `old` or
  `new` bytes (the smaller one), frees `src` and returns the new block. `src`
  must be `ptr.null` or a block from `ptr.alloc`. All pointers into `src` are
  dangling after the call.
- An empty array has no items block (`array.items` is `ptr.null`). When
  `array.append`, `array.insert`, `array.insert_many` or `array.extend` need
  more capacity, the items move to a new block and the old block is freed. The
  new capacity is two times the old capacity (`array.SIZE` for an empty
  array), or the new count if that is larger. Pointers from `array.get` and
  `array.last`, and headers that share the items of the array, are not valid
  after a growth. The item or source array can point into the destination
  array: the functions copy the source bytes before they move or free
  anything. An array that wraps borrowed memory, such as `argv`, must use
  `array.copy` before it grows.
- Temporary buffers of the standard library are freed. Except for realloc
  and array growth as described above, the functions do not free memory
  that the caller gives them.

The free checks are best effort. `ptr.free` first finds the region of the
pointer. It reads only the region headers for this, so it does not read
memory at a pointer that is not in a region. It stops the program for
pointers that are not 8 byte aligned, not in the blocks of a region (for
example the address of a literal, a scalar cell, the stack, or memory from C
`malloc`), with a bad header (also a block that goes past the end of its
region), or already free. It cannot find all bad pointers. The result of a
free of any other pointer, for example a pointer into a block, is not
defined.
