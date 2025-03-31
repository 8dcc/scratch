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
 * Structure representing a circular integer queue that can wrap around itself,
 * providing capacity for 'size' elements.
 *
 * Elements will be pushed (enqueued) to the 'back', and popped (dequeued) from
 * the 'front'.
 */
typedef struct int_queue {
    int* data;
    size_t size;
    size_t front;
    size_t back;
} int_queue_t;

/*
 * Return true if the specified queue is full.
 */
bool queue_is_full(int_queue_t* queue) {
    /*
     * The list is full if:
     *   a) The list starts in the first position, and ends in the last
     *      position.
     *   b) The 'back' index of the list wraps around the start, and it ended up
     *      right before the 'front' index.
     *
     * FIXME: According to this method, a full list stores a value in
     * 'queue->data[queue->back]', but this is not currently the case, since the
     * 'back' index indicates where the next element should be enqueued, not
     * where the last element was enqueued.
     */
    return (queue->front == 0) ? queue->back == queue->size - 1
                               : queue->back == queue->front - 1;
}

/*
 * Return true if the specified queue is empty.
 */
bool queue_is_empty(int_queue_t* queue) {
    /*
     * The list is empty if the 'back' index of the list is equal to the 'front'
     * index.
     */
    return queue->back == queue->front;
}

/*
 * Initialize the specified integer queue with the specified size. The list is
 * allocated on the heap, and must be freed with 'queue_finish'.
 *
 * Returns true on success, and false otherwise.
 */
bool queue_init(int_queue_t* queue, size_t size) {
    queue->data = calloc(size, sizeof(queue->data[0]));
    if (queue->data == NULL)
        return false;

    queue->size  = size;
    queue->front = queue->back = 0;
    return true;
}

/*
 * Deinitialize the specified queue, freeing the necessary heap-allocated
 * members. Doesn't free the queue structure itself.
 */
void queue_finish(int_queue_t* queue) {
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
bool queue_enqueue(int_queue_t* queue, int value) {
    if (queue_is_full(queue))
        return false;

    queue->data[queue->back] = value;
    if (queue->back >= queue->size - 1)
        queue->back = 0;
    else
        queue->back++;

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
int queue_dequeue(int_queue_t* queue) {
    if (queue_is_empty(queue))
        return -1;

    const int result = queue->data[queue->front];
    if (queue->front >= queue->size - 1)
        queue->front = 0;
    else
        queue->front++;

    return result;
}

/*
 * Dump the values in the specified queue, in the logical order in which they
 * would be dequeued (front -> back).
 */
void queue_dump(FILE* fp, int_queue_t* queue) {
    if (queue_is_empty(queue)) {
        fprintf(fp, "<empty>");
        return;
    }

    fprintf(fp, "[ ");

    size_t i = queue->front;
    for (;;) {
        fprintf(fp, "%d", queue->data[i]);

        if (i >= queue->size - 1)
            i = 0;
        else
            i++;

        if (i == queue->back)
            break;

        fprintf(fp, ", ");
    }

    fprintf(fp, " ]");
}

int main(void) {
    int_queue_t my_queue;
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

    return 0;
}
