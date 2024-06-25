/*
 * Given an array of N elements, for example 4, you can use 2 nested loops to
 * print all possible pairs.
 *
 *     #define ARR_SZ 3
 *     int arr[ARR_SZ] = { 'a', 'b', 'c' };
 *     for (int i = 0; i < ARR_SZ; i++)
 *         for (int j = 0; j < ARR_SZ; j++)
 *             printf("%c %c\n", arr[i], arr[j]);
 *
 * The above code would print the following 9 elements (3^2):
 *
 *     a a
 *     a b
 *     a c
 *     b a
 *     b b
 *     b c
 *     c a
 *     c b
 *     c c
 *
 * What if, instead of pairs, we wanted to print all the possible groups of 3?
 * And what if we didn't know the "group size" until runtime? This program
 * illustrates a possible solution to this problem, by allocating an array of
 * integers used to store the indexes of each of these "nested loops" we are
 * trying to simulate. Therefore, `indexes[0]' would store the value of `i' in
 * my previous example, `indexes[1]' would correspond to `j', and so on.
 */

#include <stdio.h>  /* printf(), putchar() */
#include <stdlib.h> /* calloc() */

#define ARR_SZ 4

int main(void) {
    int arr[ARR_SZ] = { 'a', 'b', 'c', 'd' };

    /* NOTE: Change the `max_nesting' variable to see the difference */
    const int max_nesting = 3;
    int* indexes          = calloc(max_nesting, sizeof(int));

    /* While the outer-most loop isn't done */
    while (indexes[0] < ARR_SZ) {
        /* NOTE: This would be the body of the innermost loop. It can access all
         * the loop counters by traversing the `indexes' array. */
        for (int i = 0; i < max_nesting; i++)
            printf("%c ", arr[indexes[i]]);
        putchar('\n');

        /* Increase necessary counters from the inner-most to the outer-most */
        for (int cur_idx = max_nesting - 1; cur_idx >= 0; cur_idx--) {
            if (cur_idx == 0 || indexes[cur_idx] + 1 < ARR_SZ) {
                indexes[cur_idx]++;
                break;
            }

            indexes[cur_idx] = 0;
        }
    }

    free(indexes);
    return 0;
}
