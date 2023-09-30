
#include <stdio.h>
#include <math.h> /* cosf, sinf, M_PI */

#define DEG2RAD(X) ((X)*M_PI / 180.0f)
#define RAD2DEG(X) ((X)*180.0f / M_PI)

#define ARR_H 20
#define ARR_W 20

typedef struct {
    int x, y;
} vec2_t;

static void rotate(float rad_ang, vec2_t in, vec2_t* out) {
    out->x = in.x * cosf(rad_ang) - in.y * sinf(rad_ang);
    out->y = in.x * sinf(rad_ang) + in.y * cosf(rad_ang);
}

static void rotate_rel(float rad_ang, vec2_t vertex, vec2_t in, vec2_t* out) {
    /* Subtract in.y from vertex.y because more Y means less index in array */
    const vec2_t rel = {
        .x = in.x - vertex.x,
        .y = vertex.y - in.y,
    };

    /* Rotate ralative */
    rotate(rad_ang, rel, out);

    /* Convert back to real array index */
    out->x = vertex.x + out->x;
    out->y = vertex.y - out->y;
}

int main() {
    char arr[ARR_H][ARR_W];
    for (int y = 0; y < ARR_H; y++)
        for (int x = 0; x < ARR_W; x++)
            arr[y][x] = '.';

    vec2_t center           = { 10, 10 };
    arr[center.y][center.x] = 'O';

    vec2_t a      = { 5, 5 };
    arr[a.y][a.x] = 'A';

    vec2_t b;
    rotate_rel(DEG2RAD(45.f), center, a, &b);
    arr[b.y][b.x] = 'B';

    vec2_t c;
    rotate_rel(DEG2RAD(-135.f), center, a, &c);
    arr[c.y][c.x] = 'C';

    /* Show array */
    for (int y = 0; y < ARR_H; y++) {
        for (int x = 0; x < ARR_W; x++)
            printf("%c", arr[y][x]);
        putchar('\n');
    }

    return 0;
}
