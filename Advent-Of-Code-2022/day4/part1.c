
#include <stdio.h>

typedef struct pair {
    int a;
    int b;
} pair;

int is_contained(pair a, pair b);

int main() {
    int n1, n2, n3, n4;
    int total = 0;

    while (scanf("%d-%d,%d-%d\n", &n1, &n2, &n3, &n4) != EOF) {
        pair p1 = { n1, n2 };
        pair p2 = { n3, n4 };
        if (is_contained(p1, p2) || is_contained(p2, p1))
            total++;
    }

    printf("Total: %d\n", total);

    return 0;
}

/* is_contained: checks if p1 contains p2 */
int is_contained(pair p1, pair p2) {
    return (p1.a <= p2.a && p1.b >= p2.b);
}
