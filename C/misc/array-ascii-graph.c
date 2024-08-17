
#include <stdio.h>

#define LENGTH(ARR) (int)(sizeof(ARR) / sizeof((ARR)[0]))

static void print_graph(int* data, int data_sz) {
    /* Number of spaces between each value in the X axis. Needed since character
     * graphs are not squares. */
    const int x_spacing = 2;

    /* Get max and min values */
    int min, max;
    min = max = data[0];
    for (int i = 1; i < data_sz; i++) {
        if (min > data[i])
            min = data[i];
        else if (max < data[i])
            max = data[i];
    }

    for (int y = max; y >= min; y--) {
        /* Print odd values of Y axis */
        if (y % 2 == 0)
            printf("%2d ", y);
        else
            printf("   ");

        /* Print the Y axis */
        printf("| ");

        /* Print the value */
        for (int x = 0; x < data_sz; x++) {
            if (data[x] == y)
                putchar('*');
            else
                putchar(' ');

            for (int i = 0; i < x_spacing; i++)
                putchar(' ');
        }

        putchar('\n');
    }

    /* Print the X axis */
    printf("   +-");
    for (int x = 0; x < data_sz * (1 + x_spacing); x++)
        putchar('-');
    putchar('\n');

    /* And the values in the X axis (positions in array) */
    printf("     ");
    for (int x = 0; x < data_sz; x++) {
        printf("%d", x);
        for (int i = 0; i < x_spacing; i++)
            putchar(' ');
    }
    putchar('\n');
}

int main(void) {
    int data[] = { 7, 1, 5, 3, 6, 4 };
    print_graph(data, LENGTH(data));

    return 0;
}
