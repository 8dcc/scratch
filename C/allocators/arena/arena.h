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
    size_t offset;
    size_t size;
} Arena;

Arena arena_new(size_t arena_sz);
void arena_destroy(Arena arena);

void* arena_alloc(Arena* arena, size_t sz);
void* arena_alloc_aligned(Arena* arena, size_t sz, size_t alignment);
