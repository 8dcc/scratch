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

#include <stddef.h>

/*
 * We need to define the `Arena' structure on the header so we can return the
 * whole instance, instead of just a pointer.
 */
typedef struct {
    char* data;
    size_t off_front;
    size_t off_back;
    size_t size;
} Arena;

/*
 * NOTE: Since this is meant to be a PoC, comments for these functions can be
 * found in the source file. In any case, more comments are available in the
 * 'arena' directory.
 */
Arena arena_new(size_t arena_sz);
void arena_destroy(Arena arena);

/*
 * NOTE: Aligned versions are not implemented in this version, but shouldn't be
 * too hard.
 */
void* arena_alloc_front(Arena* arena, size_t sz);
void* arena_alloc_back(Arena* arena, size_t sz);