
#include <stdio.h>
#include <stdlib.h>

#define MAXSZ 3

static inline void shift(int* arr, int idx);
static int sum(int* arr);

int main() {
    int total  = 0;        // Total calories of the current elf
    int max[3] = { 0 };    // Total calories of the top 3. Lower idx means bigger

    char buf[255] = { 0 };
    while (fgets(buf, 255, stdin)) {
        // Empty line
        if (buf[0] == '\n') {
            for (int i = 0; i < MAXSZ; i++) {
                if (total > max[i]) {
                    shift(max, i);
                    max[i] = total;
                    break;
                }
            }

            total = 0;
        } else {
            total += atoi(buf);
        }
    }

    printf("Total: %d\n", sum(max));

    return 0;
}

void shift(int* arr, int idx) {
    if (idx < 0)
        return;

    // From last item to idx, shift, then set idx to 0
    for (int i = MAXSZ - 1; i > idx; i--)
        arr[i] = arr[i - 1];

    arr[idx] = 0;
}

int sum(int* arr) {
    int ret = 0;

    for (int i = 0; i < MAXSZ; i++)
        ret += arr[i];

    return ret;
}
