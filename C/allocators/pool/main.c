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
 * NOTE: An improved version of this allocator is available at:
 * https://github.com/8dcc/libpool
 */

#include <stdio.h>

#include "pool.h"

/*
 * Number of `Chunk' structures available for allocation. This is not a big
 * number, since this is just a PoC.
 */
#define POOL_SZ 50

/*
 * We just have to make sure the items we store in the returned pointer are
 * small enough to fit in a chunk. In this case, smaller than 64 bytes.
 */
typedef struct MyObject {
    long long n;
    double f;
} MyObject;

int main(void) {
    /*
     * Initialize the pool once. We don't even need to understand how it's
     * implemented. Just how many chunks it has, and how big is each chunk. In
     * this example, the chunk size is hard-coded in <pool.c>.
     */
    Pool* pool = pool_new(POOL_SZ);

    /*
     * Some example allocations.
     */
    MyObject* obj1 = pool_alloc(pool);
    obj1->n        = 123;
    obj1->f        = 5.0;
    printf("Data of allocated object: %lld, %f\n", obj1->n, obj1->f);
    pool_free(pool, obj1);

    MyObject* obj2 = pool_alloc(pool);
    MyObject* obj3 = pool_alloc(pool);
    pool_free(pool, obj3);
    pool_free(pool, obj2);

    MyObject* obj4 = pool_alloc(pool);
    pool_free(pool, obj4);

    /*
     * Keep allocating until we run out of chunks. We are "leaking" pool memory
     * in this loop, but it's not leaked to the system because we will `close'
     * the pool later.
     */
    for (int i = 0;; i++) {
        if (pool_alloc(pool) == NULL) {
            printf("Failed to allocate chunk at iteration: %d (pool size: "
                   "%d)\n",
                   i, POOL_SZ);
            break;
        }
    }

    /*
     * When we are done, we "close" the pool. All data becomes unusable.
     */
    pool_close(pool);

    return 0;
}
