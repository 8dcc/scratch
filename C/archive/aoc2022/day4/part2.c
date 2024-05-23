
#include <stdio.h>

typedef struct pair {
    int a;
    int b;
} pair;

int is_overlapped(pair a, pair b);

int main() {
    int n1, n2, n3, n4;
    int total = 0;

    while (scanf("%d-%d,%d-%d\n", &n1, &n2, &n3, &n4) != EOF) {
        pair p1 = { n1, n2 };
        pair p2 = { n3, n4 };
        if (is_overlapped(p1, p2))
            total++;
    }

    printf("Total: %d\n", total);

    return 0;
}

/* is_overlapped: checks if p1 or p2 overlap the other */
int is_overlapped(pair p1, pair p2) {
    /*
     * if
     *   ....---.
     *   ..---...
     * or
     *   ..---...
     *   ...---..
     */
    return ((p1.a >= p2.a && p1.a <= p2.b) || (p2.a >= p1.a && p2.a <= p1.b));
}
