/* [[file:acl-chapter4.org::*Chapter 4.2: Binary Search][Chapter 4.2: Binary Search:2]] */
#include <stdio.h>

#define ROUND(f) (f + 0.5)  /* Redpilled sigma male round */
#define NIL (-1)

int finder(int obj, int vec[], unsigned start, unsigned end) {    /* (1) */
    const int range = end - start;                                /* (2) */

    if (range == 0) {                                             /* (3) */
        if (obj == vec[start])                                    /* (4) */
            return start;
        else
            return NIL;
    } else {
        const int mid     = start + ROUND(range / 2.0);           /* (5) */
        const int cur_obj = vec[mid];                             /* (6) */

        if (obj < cur_obj)                                        /* (7) */
            return finder(obj, vec, start, mid - 1);
        else if (obj > cur_obj)                                   /* (8) */
            return finder(obj, vec, mid + 1, end);
        else
            return mid;
    }
}

int bin_search(int obj, int vec[]) {
    const int len = 17;

    if (len == 0)
        return NIL;
    else
        return finder(obj, vec, 0, len - 1);
}

int main() {
    int test_vec[] = { 1, 3, 4, 6, 7, 8, 10, 13, 14, 18, 19, 21, 24, 27, 40, 45, 71 };

    printf("Value Index\n");
    printf("-----|-----\n");
    printf("41 %d\n", bin_search(41, test_vec));
    printf("40 %d\n", bin_search(40, test_vec));
    printf("7  %d\n", bin_search(7, test_vec));
}
/* Chapter 4.2: Binary Search:2 ends here */
