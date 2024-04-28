/* See: https://8dcc.github.io/reversing/understanding-call-stack.html */

#include <stdio.h>

extern void AsmPrintCallStack(int depth);

void DrawPoint(int x, int y) {
    printf("Drawing point: %d, %d\n", x, y);

    AsmPrintCallStack(3);
}

void DrawLine(int x0, int y0, int x1, int y1) {
    for (int y = y0; y <= y1; y++)
        for (int x = x0; x <= x1; x++)
            DrawPoint(x, y);
}

void DrawSquare(int x, int y, int w, int h) {
    int x1 = x + w;
    int y1 = y + h;

    DrawLine(x, y, x1, y);   /* Up */
    DrawLine(x, y, x, y1);   /* Left */
    DrawLine(x, y1, x1, y1); /* Down */
    DrawLine(x1, y, x1, y1); /* Right */
}

/*----------------------------------------------------------------------------*/

int main(void) {
    printf("(main) Address of DrawPoint:  %p\n", DrawPoint);
    printf("(main) Address of DrawLine:   %p\n", DrawLine);
    printf("(main) Address of DrawSquare: %p\n", DrawSquare);
    printf("(main) Address of main:       %p\n", main);

    /* DrawPoint only gets called once */
    DrawSquare(5, 5, 0, 0);

    return 0;
}
