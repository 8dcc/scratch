#include <math.h>

double my_fmod(double a, double b) {
    int bNegativeResult = a < 0;

    a = fabs(a);
    b = fabs(b);

    while (a >= b)
        a -= b;

    return bNegativeResult ? -a : a;
}

double my_emacs_fmod(double a, double b) {
    a = my_fmod(a, b);

    /* If the "remainder" comes out with the wrong sign, fix it. */
    if (b < 0 ? a > 0 : a < 0)
        a += b;

    return a;
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

    printf("fmod(9 4)       -> %.2f\n", fmod(9, 4));
    printf("fmod(-9 4)      -> %.2f\n", fmod(-9, 4));
    printf("fmod(9 -4)      -> %.2f\n", fmod(9, -4));
    printf("fmod(-9 -4)     -> %.2f\n", fmod(-9, -4));
    printf("fmod(9.5 2.5)   -> %.2f\n", fmod(9.5, 2.5));
    printf("fmod(-9.5 2.5)  -> %.2f\n", fmod(-9.5, 2.5));
    printf("fmod(9.5 -2.5)  -> %.2f\n", fmod(9.5, -2.5));
    printf("fmod(-9.5 -2.5) -> %.2f\n", fmod(-9.5, -2.5));

    printf("my_emacs_fmod(9 4)       -> %.2f\n", my_emacs_fmod(9, 4));
    printf("my_emacs_fmod(-9 4)      -> %.2f\n", my_emacs_fmod(-9, 4));
    printf("my_emacs_fmod(9 -4)      -> %.2f\n", my_emacs_fmod(9, -4));
    printf("my_emacs_fmod(-9 -4)     -> %.2f\n", my_emacs_fmod(-9, -4));
    printf("my_emacs_fmod(9.5 2.5)   -> %.2f\n", my_emacs_fmod(9.5, 2.5));
    printf("my_emacs_fmod(-9.5 2.5)  -> %.2f\n", my_emacs_fmod(-9.5, 2.5));
    printf("my_emacs_fmod(9.5 -2.5)  -> %.2f\n", my_emacs_fmod(9.5, -2.5));
    printf("my_emacs_fmod(-9.5 -2.5) -> %.2f\n", my_emacs_fmod(-9.5, -2.5));

    return 0;
}
