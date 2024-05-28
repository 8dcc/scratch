#include <stdio.h>
#include <stddef.h>

int find_smallest_int(int *vec, size_t len)
{
    int m = *vec;
    for (unsigned long n = 0; n < len; n++) {
        printf("%d ", vec[n]);
        if (vec[n] > m) m = vec[n];
    }
    return m;
}

int main() {
    int nums[5] = {4,5,6,7,8};
    int *p = nums;

    printf("\n%d\n", find_smallest_int(p, 5));

    return 0;
}
