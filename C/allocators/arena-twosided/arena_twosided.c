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
 *
 * ----------------------------------------------------------------------------
 *
 * This is a modified version of the arena allocator found in this 'scratch'
 * repository, so see that code for more comments.
 *
 * This version, however, allows the user to allocate from the "front" and the
 * "back". This can be useful for managing objects with different lifetimes,
 * from the same arena.
 *
 * The "front" and "back" regions shared the whole arena space, so if one grows
 * less, the other can grow more.
 *
 *                          (Arena size)
 *                        <--------------->
 *     ###################.......      ....######################
 *                        ^      ^     ^  ^
 *                        |      |     |  |
 *     (Bottom of front) -+      |     |  +- (Bottom of back)
 *                               |     |
 *               (Top of front) -+     +- (Top of back)
 *
 * Where '#' represents unusable space, '.' means allocated chunks, and ' '
 * means unallocated chunks in the arena.
 *
 * Notice how the top of "back" essentially points to the last returned
 * pointer. A more visual example can be seen with the `print_twosided_arena'
 * function in 'main.c'.
 *
 * For more information, see Knuth's Art of Computer Programming, Volume 1,
 * Section 2.2.2. Sequential Allocation.
 */

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>

#include "arena_twosided.h"

/*
 * Allocate and initialize a new `Arena', with the specified size.
 *
 * NOTE: This function is similar, but not identical to the one used in the
 * 'arena' directory. We also have to set `arena.off_back'.
 */
Arena arena_new(size_t arena_sz) {
    Arena arena;
    arena.size      = arena_sz;
    arena.off_front = 0;
    arena.off_back  = 0;
    arena.data      = malloc(arena.size);
    return arena;
}

/*
 * Free the contents of the specified `Arena'. The arena, along with the memory
 * chunks allocated from it using `arena_alloc', become unusable.
 *
 * NOTE: This function is identical to the one used in the 'arena' directory.
 */
void arena_destroy(Arena arena) {
    free(arena.data);
    arena.data = NULL;
}

/*----------------------------------------------------------------------------*/

/*
 * Allocate `sz' bytes of memory in the front of the specified arena, ignoring
 * alignment.
 *
 * NOTE: This function is similar, but not identical to the one used in the
 * 'arena' directory. Instead of ensuring that the requested size doesn't exceed
 * `arena.size', we have to ensure that it doesn't exceed the "back" offset.
 */
void* arena_alloc_front(Arena* arena, size_t sz) {
    if (arena->off_front + sz > arena->size - arena->off_back)
        return NULL;

    void* result = &arena->data[arena->off_front];
    arena->off_front += sz;
    return result;
}

/*
 * Allocate `sz' bytes of memory in the front of the specified arena, ignoring
 * alignment.
 *
 * Notice how the overflow conditional is very similar to the one used in
 * `arena_alloc_front', since we are storing the "back" offset as a positive
 * integer.
 *
 * Notice how we have to modify the "back" offset before using it to access
 * `arena.data'.
 */
void* arena_alloc_back(Arena* arena, size_t sz) {
    if (arena->off_back + sz > arena->size - arena->off_front)
        return NULL;

    arena->off_back += sz;
    return &arena->data[arena->size - arena->off_back];
}
