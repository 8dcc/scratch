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
 * NOTE: An improved version of this allocator is available as a stand-alone
 * library at: https://github.com/8dcc/libpool
 */

#include <stddef.h>
#include <stdlib.h>

#include "pool.h"

/*
 * In a pool allocator, each chunk has a fixed size. The size of the chunks will
 * be set with an array of characters, inside the `Chunk.v' union.
 *
 * In this case, each chunk will be 64 bytes. It's common to have many pools of
 * different chunk sizes.
 */
#define CHUNK_SZ 64

/*
 * The `Chunk' type is a union so the chunk size can be specified more
 * easily. The data in a non-free chunk will be overwritten by the user with the
 * `arr' member:
 *
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   | <user-data> |  | <user-data> |  | <user-data> |  | <user-data> |
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *
 * However, if the chunk is free, we can use the `Chunk.next' pointer to build a
 * linked list of available chunks:
 *
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   | * |         |  | * |         |  | * |         |  | X |         |
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *     |              ^ |              ^ |              ^
 *     '--------------' '--------------' '--------------'
 *
 * Where '*' represents a valid pointer and 'X' represents NULL (the end of the
 * list).
 *
 * This linked list is built once, inside this `pool_new' function, and it is
 * the only time when we have to iterate the `Chunk' array. The linked list will
 * be modified by `pool_alloc' and `pool_free', in O(1) time.
 *
 * For more information, see the comments in `pool_alloc' and `pool_free'.
 */
typedef union Chunk Chunk;
union Chunk {
    Chunk* next;
    char arr[CHUNK_SZ];
};

/*
 * The actual pool structure, which contains a pointer to the first chunk, and
 * a pointer to the start of the linked list of free chunks.
 *
 * We need to store the first chunk for freeing the actual `Chunk' array once
 * the user is done with the pool.
 *
 * The user is able to allocate with O(1) time, because the `Pool.free_chunk'
 * pointer always points to a free chunk without needing to iterate anything.
 */
struct Pool {
    Chunk* chunk_arr;
    Chunk* free_chunk;
};

/*----------------------------------------------------------------------------*/

/*
 * Allocate and initialize a new `Pool' structure, with the specified number of
 * chunks. If the initialization fails, NULL is returned. Note that, in this
 * simple pool allocator, the chunk size is hard-coded.
 *
 * In this case, we allocate both the `Pool' structure and the `Chunk' array
 * with `malloc', but you could even declare an array of chunks, and operate on
 * that.
 *
 * This initialization function is the only time when we have to iterate the
 * `Chunk' array, to initialize the `.next' pointers. When creating a new pool,
 * all chunks start free, so the `free_chunk' is the start of the linked list:
 *
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   | * |         |  | * |         |  | * |         |  | X |         |
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   ^ |              ^ |              ^ |              ^
 *   | '--------------' '--------------' '--------------'
 *   |
 *   '-- (pool.free_chunk)
 *
 * For more information on how this `free_chunk' pointer is used, see
 * `pool_alloc'.
 */
Pool* pool_new(size_t pool_sz) {
    Pool* pool = malloc(sizeof(Pool));
    if (pool == NULL)
        return NULL;

    pool->chunk_arr = pool->free_chunk = malloc(pool_sz * sizeof(Chunk));
    if (pool->chunk_arr == NULL) {
        free(pool);
        return NULL;
    }

    for (size_t i = 0; i < pool_sz - 1; i++)
        pool->chunk_arr[i].next = &pool->chunk_arr[i + 1];
    pool->chunk_arr[pool_sz - 1].next = NULL;

    return pool;
}

/*
 * Free all data in a `Pool' structure, and the structure itself. All data in
 * the pool becomes unusable. Allows NULL.
 */
void pool_close(Pool* pool) {
    if (pool == NULL)
        return;

    free(pool->chunk_arr);
    free(pool);
}

/*----------------------------------------------------------------------------*/

/*
 * Allocate a fixed-size chunk from the specified pool. If no chunks are
 * available, NULL is returned. Again, note that in this simple allocator, the
 * chunk size is hard-coded in this source file.
 *
 * The allocation process is very simple and fast. Since the `pool' has a
 * pointer to the start of a linked list of free `Chunk' items, we can just
 * return that pointer, and set the new start of the linked list to the second
 * item of the old list. Before the allocation:
 *
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   | * |         |  | * |         |  | * |         |  | X |         |
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   ^ |              ^ |              ^ |              ^
 *   | '--------------' '--------------' '--------------'
 *   |
 *   '-- (pool.free_chunk)
 *
 * And after the allocation:
 *
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   | <user-data> |  | * |         |  | * |         |  | X |         |
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *                    ^ |              ^ |              ^
 *                    | '--------------' '--------------'
 *                    |
 *                    '-- (pool.free_chunk)
 */
void* pool_alloc(Pool* pool) {
    if (pool == NULL || pool->free_chunk == NULL)
        return NULL;

    Chunk* result    = pool->free_chunk;
    pool->free_chunk = pool->free_chunk->next;
    return result;
}

/*
 * Free a fixed-size chunk from the specified pool. Allows NULL as the `ptr'
 * argument.
 *
 * Note that, since we are using a linked list, we don't have to free in
 * order. Before the free:
 *
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   | <user-data> |  | <user-data> |  | * |         |  | X |         |
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *                                     ^ |              ^
 *                                     | '--------------'
 *                                     |
 *                                     '-- (pool.free_chunk)
 *
 * After freeing the first chunk:
 *
 *   (A)              (B)              (C)              (D)
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   | * |         |  | <user-data> |  | * |         |  | X |         |
 *   +-------------+  +-------------+  +-------------+  +-------------+
 *   ^ |                               ^ |              ^
 *   | '-------------------------------' '--------------'
 *   |
 *   '-- (pool.free_chunk)
 *
 * Note how chunk B remains unchanged. If we wanted to free it, we would just
 * have to set chunk A as the `.next' pointer of chunk B.
 */
void pool_free(Pool* pool, void* ptr) {
    if (pool == NULL || ptr == NULL)
        return;

    Chunk* freed     = ptr;
    freed->next      = pool->free_chunk;
    pool->free_chunk = freed;
}
