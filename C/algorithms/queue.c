/*
 * Copyright 2025 8dcc
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

#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * Type for a single value in the queue.
 */
typedef int queue_value_t;

/*
 * Types for indexes and sizes in the queue.
 */
typedef size_t queue_size_t;
typedef queue_size_t queue_idx_t;

/*
 * Structure representing a circular integer queue that can wrap around
 * itself. Elements will be pushed (enqueued) to the 'back', and popped
 * (dequeued) from the 'front'.
 *
 * The 'capacity' stores the size of the 'data' array, and 'count' stores the
 * actual difference between 'back' and 'front'.
 */
typedef struct queue {
    queue_value_t* data;
    queue_size_t capacity;
    queue_size_t count;
    queue_idx_t front;
    queue_idx_t back;
} queue_t;

/*
 * Return the next (incremented) value for the specified index in the queue,
 * wrapping around the end when necessary.
 */
static inline queue_idx_t get_next_idx(const queue_t* queue,
                                       queue_idx_t cur_idx) {
    return (cur_idx >= queue->capacity - 1) ? 0 : cur_idx + 1;
}

/*
 * Return the previous (decremented) value for the specified index in the queue,
 * wrapping around the start when necessary.
 */
static inline queue_idx_t get_prev_idx(const queue_t* queue,
                                       queue_idx_t cur_idx) {
    return (cur_idx == 0) ? queue->capacity - 1 : cur_idx - 1;
}

/*
 * Return true if the specified queue is full.
 */
bool queue_is_full(const queue_t* queue) {
    return queue->count == queue->capacity;
}

/*
 * Return true if the specified queue is empty.
 */
bool queue_is_empty(const queue_t* queue) {
    return queue->count == 0;
}

/*
 * Initialize the specified integer queue with the specified capacity. The list
 * is allocated on the heap, and must be freed with 'queue_destroy'.
 *
 * Returns true on success, and false otherwise.
 */
bool queue_init(queue_t* queue, queue_size_t capacity) {
    queue->data = calloc(capacity, sizeof(queue->data[0]));
    if (queue->data == NULL)
        return false;

    queue->capacity = capacity;
    queue->count    = 0;
    queue->front    = 0;
    queue->back     = 0;

    return true;
}

/*
 * Deinitialize the specified queue, freeing the necessary heap-allocated
 * members. Doesn't free the queue structure itself.
 */
void queue_destroy(queue_t* queue) {
    if (queue->data != NULL) {
        free(queue->data);
        queue->data = NULL;
    }
}

/*
 * Add the specified value to the back of the specified queue.
 *
 * Returns true on success, or false on error (e.g. if the queue is full).
 */
bool queue_enqueue(queue_t* queue, queue_value_t value) {
    if (queue_is_full(queue))
        return false;

    queue->data[queue->back] = value;
    queue->back              = get_next_idx(queue, queue->back);
    queue->count++;

    return true;
}

/*
 * Pop the value from the front of the specified queue.
 *
 * Returns the value at the front on success, or -1 on error (e.g. if the queue
 * is empty).
 *
 * FIXME: Return a unique value that can't be normally inside the queue.
 */
queue_value_t queue_dequeue(queue_t* queue) {
    if (queue_is_empty(queue))
        return -1;

    const int result = queue->data[queue->front];
    queue->front     = get_next_idx(queue, queue->front);
    queue->count--;

    return result;
}

/*
 * Dump the values in the specified queue, in the logical order in which they
 * would be dequeued (front -> back).
 */
void queue_dump(FILE* fp, const queue_t* queue) {
    if (queue_is_empty(queue)) {
        fprintf(fp, "<empty>");
        return;
    }

    fprintf(fp, "[ ");

    queue_idx_t real_idx = queue->front;
    for (queue_idx_t i = 0; i < queue->count; i++) {
        fprintf(fp, "%d", queue->data[real_idx]);

        real_idx = get_next_idx(queue, real_idx);
        if (real_idx == queue->back)
            break;

        fprintf(fp, ", ");
    }

    fprintf(fp, " ]");
}

int main(void) {
    queue_t my_queue;
    queue_init(&my_queue, 10);
    assert(queue_is_empty(&my_queue));

    printf("Arbitrary operations:\n");
    queue_enqueue(&my_queue, 123);            /* [ ]          -> [ 123 ]      */
    queue_enqueue(&my_queue, 456);            /* [123]        -> [ 123, 456 ] */
    printf("%d\n", queue_dequeue(&my_queue)); /* [ 123, 456 ] -> [ 456 ]      */
    queue_enqueue(&my_queue, 789);            /* [ 456 ]      -> [ 456, 789 ] */
    printf("%d\n", queue_dequeue(&my_queue)); /* [ 456, 789 ] -> [ 789 ]      */
    printf("%d\n", queue_dequeue(&my_queue)); /* [ 789 ]      -> [ ]          */

    /* Fill the queue */
    for (int i = 0; queue_enqueue(&my_queue, i) != false; i++)
        ;
    assert(queue_is_full(&my_queue));

    printf("Dumping full queue: ");
    queue_dump(stdout, &my_queue);
    putchar('\n');

    /* Empty the queue */
    while (queue_dequeue(&my_queue) != -1)
        ;
    assert(queue_is_empty(&my_queue));

    printf("Dumping empty queue: ");
    queue_dump(stdout, &my_queue);
    putchar('\n');

    queue_destroy(&my_queue);
    return 0;
}
