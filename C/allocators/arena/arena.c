/*
 * Copyright 2024 8dcc
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
 */

/*
 * What is an arena allocator?
 * ---------------------------
 *
 * This is a simple implementation of an arena allocator. Quoting Cris Wellons:
 *
 *   > An arena is a memory buffer and an offset into that buffer, initially
 *   > zero. To allocate an object, grab a pointer at the offset, advance the
 *   > offset by the size of the object, and return the pointer. [...] Objects
 *   > are not freed individually. Instead, groups of allocations are freed at
 *   > once by restoring the offset to an earlier value.
 *
 * His article (linked below) has more information on how arena allocators work.
 *
 * How is the memory freed?
 * ------------------------
 *
 * The pointers returned by `arena_alloc' do not need to be freed explicitly.
 * The caller "frees" allocated blocks by restoring an `Arena' structure to its
 * previous state. This is often done by passing the `Arena' header to a
 * function by copy, instead of by reference.
 *
 * For example, some function `foo' might pass an `Arena' argument (that is, the
 * whole structure, not a pointer to it) to another function `bar'. All the
 * internal allocations made from `bar' will be "freed" implicitly when
 * returning to `foo', because the `Arena' structure from `foo' was not
 * affected.
 *
 * Credits:
 * --------
 *
 * https://www.rfleury.com/p/untangling-lifetimes-the-arena-allocator
 * https://nullprogram.com/blog/2023/09/27/
 */

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>

#include "arena.h"

/*
 * The PADDING_FOR_ALIGNMENT macro calculates the number of bytes that should be
 * added to OFFSET in order for it to be aligned to ALIGNMENT, as long as
 * ALIGNMENT is a power of two.
 *
 * Quoting Section 3-1 of Hacker's Delight, by Henry S. Warren, Jr.:
 *
 *   > An unsigned integer `x' can be rounded up to the next greater multiple of
 *   > 8 with either of:
 *   >
 *   >   (x + 7) & -8
 *   >   x + (-x & 7)
 *   >
 *   > The second term of the second expression is useful if you want to know
 *   > how much you must add to `x' to make it a multiple of 8.
 *
 * Example of how this works, with the number 23:
 *
 *   |                    | Alignment     | Offset         |
 *   |--------------------+---------------+----------------|
 *   | Initial input      | 00010000 (16) | 00010111 (23)  |
 *   | Negate offset      | -             | 11101001 (-23) |
 *   | Decrease alignment | 00001111 (15) | -              |
 *   | Bit-wise AND       | -             | 00001001 (9)   |
 *
 * And if `x' was already aligned:
 *
 *   |                    | Alignment     | Offset         |
 *   |--------------------+---------------+----------------|
 *   | Initial input      | 00010000 (16) | 00100000 (32)  |
 *   | Negate offset      | -             | 11100000 (-32) |
 *   | Decrease alignment | 00001111 (15) | -              |
 *   | Bit-wise AND       | -             | 00000000 (0)   |
 *
 * We can't just do (ALIGNMENT - (OFFSET % ALIGNMENT)), because OFFSET might be
 * already aligned, and that method would return ALIGNMENT.
 */
#define PADDING_FOR_ALIGNMENT(OFFSET, ALIGNMENT) (-(OFFSET) & ((ALIGNMENT)-1))

/*----------------------------------------------------------------------------*/

/*
 * Allocate and initialize a new `Arena', with the specified size.
 *
 * The actual `Arena' structure is allocated on the stack, but the memory area
 * is allocated externally. The contents of the returned `Arena' must be freed
 * by the caller using `arena_destroy'.
 *
 * NOTE: The definition of the `Arena' structure is inside "arena.h", because we
 * return the whole structure, not just a pointer.
 *
 * NOTE: In this case, the external allocation method is hard-coded to be
 * stdlib's `malloc', but it could be changed.
 */
Arena arena_new(size_t arena_sz) {
    Arena arena;
    arena.size   = arena_sz;
    arena.offset = 0;

    /*
     * NOTE: We could use an external function for allocating the actual arena.
     */
    arena.data = malloc(arena.size);

    return arena;
}

/*
 * Free the contents of the specified `Arena'. The arena, along with the memory
 * blocks allocated from it using `arena_alloc', become unusable.
 */
void arena_destroy(Arena arena) {
    /*
     * NOTE: We could use an external function for freeing the actual arena.
     */
    free(arena.data);
    arena.data = NULL;
}

/*----------------------------------------------------------------------------*/

/*
 * Allocate `sz' bytes of memory in the specified arena.
 *
 * Note that the pointer returned by `arena_alloc' does not have to be
 * aligned. If the caller expects an aligned pointer, use `arena_alloc_aligned'.
 *
 * For details on how the allocated blocks are "freed", see the comment at the
 * top of this source file.
 */
void* arena_alloc(Arena* arena, size_t sz) {
    /*
     * Not enough memory in arena. Instead of returning NULL, we could
     * reallocate the arena, for example.
     */
    if (arena->offset + sz >= arena->size)
        return NULL;

    void* result = &arena->data[arena->offset];
    arena->offset += sz;
    return result;
}

/*
 * Same as `arena_alloc', but the returned pointer must be aligned to
 * `alignment'.
 */
void* arena_alloc_aligned(Arena* arena, size_t sz, size_t alignment) {
    /*
     * Calculate the padding needed for `offset', so that it's aligned with the
     * `alignment' argument.
     *
     * FIXME: This assumes that `arena->data' is also aligned to
     * `ALLOC_ALIGNMENT'. We shouldn't assume this.
     */
    const size_t missing_padding =
      PADDING_FOR_ALIGNMENT(arena->offset, alignment);

    if (arena->offset + missing_padding >= arena->size)
        return NULL;

    arena->offset += missing_padding;
    return arena_alloc(arena, sz);
}
