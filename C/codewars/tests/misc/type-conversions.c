#include <stdio.h>

float f_func(float);

int main() {
    float ftest = 6.9;
    printf("Orig: %f | f_func: %f | (int)f_func: %d", ftest, f_func(ftest), (int)f_func(ftest));
    return 0;
}

float f_func(float n) {
    return n + 2.5;
}
