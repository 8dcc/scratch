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

#ifndef POOL_H_
#define POOL_H_ 1

#include <stddef.h>

/* For more information and comments, see <pool.c> */

typedef struct Pool Pool;

Pool* pool_new(size_t pool_sz);
void pool_close(Pool* pool);

void* pool_alloc(Pool* pool);
void pool_free(Pool* pool, void* ptr);

#endif /* POOL_H_ */
