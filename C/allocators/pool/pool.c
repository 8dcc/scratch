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
 *
 * NOTE: See also my blog article about pool allocators at:
 * https://8dcc.github.io/programming/pool-allocator.html
 */

#include <stddef.h>
#include <stdbool.h>
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
 * Linked list of pointers, used to store the start of the chunk arrays inside a
 * pool.
 *
 * We need to store them as a linked list, since there can be an arbitrary
 * number of them, one for each call to `pool_resize' plus the initial one from
 * `pool_new'. New pointers will be prepended to the linked list.
 */
typedef struct LinkedPtr LinkedPtr;
struct LinkedPtr {
    LinkedPtr* next;
    Chunk* ptr;
};

/*
 * The actual pool structure, which contains a pointer to the first chunk, and
 * a pointer to the start of the linked list of free chunks.
 *
 * We need to store a list of array starts for freeing the actual `Chunk' arrays
 * once the user is done with the pool.
 *
 * The user is able to allocate with O(1) time, because the `Pool.free_chunk'
 * pointer always points to a free chunk without needing to iterate anything.
 */
struct Pool {
    Chunk* free_chunk;
    LinkedPtr* array_starts;
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

    Chunk* arr = pool->free_chunk = malloc(pool_sz * sizeof(Chunk));
    if (arr == NULL) {
        free(pool);
        return NULL;
    }

    for (size_t i = 0; i < pool_sz - 1; i++)
        arr[i].next = &arr[i + 1];
    arr[pool_sz - 1].next = NULL;

    pool->array_starts = malloc(sizeof(LinkedPtr));
    if (pool->array_starts == NULL) {
        free(arr);
        free(pool);
        return NULL;
    }

    pool->array_starts->next = NULL;
    pool->array_starts->ptr  = arr;

    return pool;
}

/*
 * Resize the specified `pool', adding `extra_chunk_num' free chunks.
 *
 * Resizing the pool simply means allocating a new chunk array, and prepending
 * it to the `pool->free_chunk' linked list.
 */
bool pool_resize(Pool* pool, size_t extra_chunk_num) {
    if (pool == NULL || extra_chunk_num == 0)
        return false;

    Chunk* extra_chunk_arr = malloc(extra_chunk_num * sizeof(Chunk));
    if (extra_chunk_arr == NULL)
        return false;

    /* Link the new free chunks together */
    for (size_t i = 0; i < extra_chunk_num - 1; i++)
        extra_chunk_arr[i].next = &extra_chunk_arr[i + 1];

    /* Prepend the new chunk array to the linked list of free chunks */
    extra_chunk_arr[extra_chunk_num - 1].next = pool->free_chunk;
    pool->free_chunk                          = extra_chunk_arr;

    LinkedPtr* array_start = malloc(sizeof(LinkedPtr));
    if (array_start == NULL) {
        free(extra_chunk_arr);
        return false;
    }

    /* Prepend to the linked list of array starts */
    array_start->ptr   = extra_chunk_arr;
    array_start->next  = pool->array_starts;
    pool->array_starts = array_start;

    return true;
}

/*
 * Free all data in a `Pool' structure, and the structure itself. All data in
 * the pool becomes unusable. Allows NULL.
 */
void pool_close(Pool* pool) {
    if (pool == NULL)
        return;

    LinkedPtr* linkedptr = pool->array_starts;
    while (linkedptr != NULL) {
        LinkedPtr* next = linkedptr->next;

        /*
         * Free the current chunk array, and the `LinkedPtr' structure itself.
         */
        free(linkedptr->ptr);
        free(linkedptr);

        linkedptr = next;
    }

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
