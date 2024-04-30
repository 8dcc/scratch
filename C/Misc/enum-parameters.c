
#include <stdio.h>

enum test_enum {
    A = 1,
    B,
    C,
    D,
    E,
};

#define F 6

static void print_enum(enum test_enum n) {
    printf("Printing enum: %d\n", n);
}

int main() {
    print_enum(A);
    print_enum(1);
    print_enum(99);
    print_enum(F);    // Outside the enum

    return 0;
}
