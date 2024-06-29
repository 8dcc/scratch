#include <math.h>

double my_fmod(double x, double y) {
    int bNegativeResult = x < 0;

    x = fabs(x);
    y = fabs(y);

    while (x >= y)
        x -= y;

    return bNegativeResult ? -x : x;
}

double my_emacs_fmod(double x, double y) {
    x = my_fmod(x, y);

    /* If the "remainder" comes out with the wrong sign, fix it. */
    if (y < 0 ? x > 0 : x < 0)
        x += y;

    return x;
}

double my_emacs_fmod2(double x, double y) {
    double abs_x = fabs(x);
    double abs_y = fabs(y);

    /* Calculate fmod(fabs(x), fabs(y)) */
    double abs_result = abs_x;
    while (abs_result >= abs_y)
        abs_result -= abs_y;

    if (x >= 0 && y >= 0)
        return abs_result;
    if (x >= 0 && y < 0)
        return y + abs_result;
    if (x < 0 && y >= 0)
        return y - abs_result;
    if (x < 0 && y < 0)
        return -abs_result;
}

#include <stdio.h>

int main(void) {
    printf("my_fmod(9 4)       -> %.2f\n", my_fmod(9, 4));
    printf("my_fmod(-9 4)      -> %.2f\n", my_fmod(-9, 4));
    printf("my_fmod(9 -4)      -> %.2f\n", my_fmod(9, -4));
    printf("my_fmod(-9 -4)     -> %.2f\n", my_fmod(-9, -4));
    printf("my_fmod(9.5 2.5)   -> %.2f\n", my_fmod(9.5, 2.5));
    printf("my_fmod(-9.5 2.5)  -> %.2f\n", my_fmod(-9.5, 2.5));
    printf("my_fmod(9.5 -2.5)  -> %.2f\n", my_fmod(9.5, -2.5));
    printf("my_fmod(-9.5 -2.5) -> %.2f\n", my_fmod(-9.5, -2.5));
    putchar('\n');
    printf("fmod(9 4)       -> %.2f\n", fmod(9, 4));
    printf("fmod(-9 4)      -> %.2f\n", fmod(-9, 4));
    printf("fmod(9 -4)      -> %.2f\n", fmod(9, -4));
    printf("fmod(-9 -4)     -> %.2f\n", fmod(-9, -4));
    printf("fmod(9.5 2.5)   -> %.2f\n", fmod(9.5, 2.5));
    printf("fmod(-9.5 2.5)  -> %.2f\n", fmod(-9.5, 2.5));
    printf("fmod(9.5 -2.5)  -> %.2f\n", fmod(9.5, -2.5));
    printf("fmod(-9.5 -2.5) -> %.2f\n", fmod(-9.5, -2.5));
    putchar('\n');
    printf("my_emacs_fmod(9 4)       -> %.2f\n", my_emacs_fmod(9, 4));
    printf("my_emacs_fmod(-9 4)      -> %.2f\n", my_emacs_fmod(-9, 4));
    printf("my_emacs_fmod(9 -4)      -> %.2f\n", my_emacs_fmod(9, -4));
    printf("my_emacs_fmod(-9 -4)     -> %.2f\n", my_emacs_fmod(-9, -4));
    printf("my_emacs_fmod(9.5 2.5)   -> %.2f\n", my_emacs_fmod(9.5, 2.5));
    printf("my_emacs_fmod(-9.5 2.5)  -> %.2f\n", my_emacs_fmod(-9.5, 2.5));
    printf("my_emacs_fmod(9.5 -2.5)  -> %.2f\n", my_emacs_fmod(9.5, -2.5));
    printf("my_emacs_fmod(-9.5 -2.5) -> %.2f\n", my_emacs_fmod(-9.5, -2.5));
    putchar('\n');
    printf("my_emacs_fmod2(9 4)       -> %.2f\n", my_emacs_fmod2(9, 4));
    printf("my_emacs_fmod2(-9 4)      -> %.2f\n", my_emacs_fmod2(-9, 4));
    printf("my_emacs_fmod2(9 -4)      -> %.2f\n", my_emacs_fmod2(9, -4));
    printf("my_emacs_fmod2(-9 -4)     -> %.2f\n", my_emacs_fmod2(-9, -4));
    printf("my_emacs_fmod2(9.5 2.5)   -> %.2f\n", my_emacs_fmod2(9.5, 2.5));
    printf("my_emacs_fmod2(-9.5 2.5)  -> %.2f\n", my_emacs_fmod2(-9.5, 2.5));
    printf("my_emacs_fmod2(9.5 -2.5)  -> %.2f\n", my_emacs_fmod2(9.5, -2.5));
    printf("my_emacs_fmod2(-9.5 -2.5) -> %.2f\n", my_emacs_fmod2(-9.5, -2.5));

    return 0;
}
