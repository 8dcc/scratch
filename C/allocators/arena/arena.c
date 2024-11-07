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
 * The ALIGN_OFFSET macro aligns an OFFSET to ALIGNMENT, as long as it's a power
 * of two. An optimized version of:
 *
 *   ALIGNMENT - (OFFSET % ALIGNMENT)
 */
#define MODULO_POWER_OF_TWO(A, B) ((A) & (B - 1))
#define ALIGN_OFFSET(OFFSET, ALIGNMENT) \
    ((ALIGNMENT)-MODULO_POWER_OF_TWO((OFFSET), (ALIGNMENT)))

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

#if 0
void* arena_alloc_aligned(Arena* arena, size_t sz, size_t alignment) {
    /*
     * We align the arena offset to `ALLOC_ALIGNMENT', so the returned pointer
     * is aligned.
     *
     * FIXME: This assumes that `arena->data' is also aligned to
     * `ALLOC_ALIGNMENT'. Don't assume this.
     */
    const size_t aligned_offset = ALIGN_OFFSET(arena->offset, alignment);
}
#endif
