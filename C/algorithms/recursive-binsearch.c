/* See https://github.com/8dcc/lisp-stuff/blob/main/src/acl-chapter4.org */

#include <stdio.h>

#define ARR_LEN  17
#define ROUND(f) (f + 0.5) /* You are not ready for this swag */
#define NIL      (-1)

int finder(int obj, int vec[], unsigned start, unsigned end) {
    const int range = end - start;

    if (range == 0) {
        if (obj == vec[start])
            return start;
        else
            return NIL;
    } else {
        const int mid     = start + ROUND(range / 2.0);
        const int cur_obj = vec[mid];

        if (obj < cur_obj)
            return finder(obj, vec, start, mid - 1);
        else if (obj > cur_obj)
            return finder(obj, vec, mid + 1, end);
        else
            return mid;
    }
}

int bin_search(int obj, int vec[]) {
    const int len = ARR_LEN; /* Make it more visual */

    if (len == 0)
        return NIL;
    else
        return finder(obj, vec, 0, len - 1);
}

int main() {
    int test_vec[ARR_LEN] = { 1,  3,  4,  6,  7,  8,  10, 13, 14,
                              18, 19, 21, 24, 27, 40, 45, 71 };

    /* Print idx */
    printf("Index: ");
    for (int i = 0; i < ARR_LEN; i++)
        printf("| %2d ", i);
    printf("|\n");

    /* Print arr */
    printf("Item:  ");
    for (int i = 0; i < ARR_LEN; i++)
        printf("| %2d ", test_vec[i]);
    printf("|\n\n");

    printf("Index of 41: %d\n", bin_search(41, test_vec));
    printf("Index of 40: %d\n", bin_search(40, test_vec));
    printf("Index of 7:  %d\n", bin_search(7, test_vec));
}
