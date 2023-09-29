
#include <stdio.h>
#include <math.h> /* cosf, sinf, M_PI */

#define DEG2RAD(X) ((X)*M_PI / 180.0f)
#define RAD2DEG(X) ((X)*180.0f / M_PI)

#define ARR_H 20
#define ARR_W 20

typedef struct {
    int x, y;
} vec2_t;

static void rotate(float rad_ang, const vec2_t* in, vec2_t* out) {
    out->x = in->x * cosf(rad_ang) - in->y * sinf(rad_ang);
    out->y = in->x * sinf(rad_ang) + in->y * cosf(rad_ang);
}

static void rotate_rel(float rad_ang, const vec2_t* orig, const vec2_t* in,
                       vec2_t* out) {
    /* Subtract in.y from orig.y because more Y means less index in array */
    const vec2_t rel = {
        .x = in->x - orig->x,
        .y = orig->y - in->y,
    };

    /* Rotate ralative */
    rotate(rad_ang, &rel, out);

    /* Convert back to real array index */
    out->x = orig->x + out->x;
    out->y = orig->y - out->y;
}

int main() {
    char arr[ARR_H][ARR_W];
    for (int y = 0; y < ARR_H; y++)
        for (int x = 0; x < ARR_W; x++)
            arr[y][x] = '.';

    /*------------------------------------------------------------------------*/
    /* MANUAL */

    /* Center */
    vec2_t orig         = { 10, 10 };
    arr[orig.y][orig.x] = 'O';

    /* Example point */
    vec2_t a      = { 15, 10 };
    arr[a.y][a.x] = 'A';

    /* Relative from orig */
    vec2_t rel = {
        .x = a.x - orig.x,
        .y = orig.y - a.y,
    };

    /* Rotate the relative position of A from Orig, and save it in B */
    vec2_t b;
    rotate(DEG2RAD(45.f), &rel, &b);

    /* B is still relative, get real positions */
    vec2_t real = {
        .x = orig.x + b.x,
        .y = orig.y - b.y,
    };
    arr[real.y][real.x] = 'B';

    /*------------------------------------------------------------------------*/
    /* RELATIVE FUNC */

    vec2_t c;
    rotate_rel(DEG2RAD(225.f), &orig, &a, &c);
    arr[c.y][c.x] = 'C';

    /* Show array */
    for (int y = 0; y < ARR_H; y++) {
        for (int x = 0; x < ARR_W; x++)
            printf("%c", arr[y][x]);
        putchar('\n');
    }

    return 0;
}
