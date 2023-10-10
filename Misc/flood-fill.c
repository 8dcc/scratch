
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define W 10
#define H 10

#define EMPTY    ' '
#define REVEALED '-'

#define GRID_SET(P, X) (grid[W * (P).y + (P).x] = (X))

typedef struct {
    int x, y;
} vec2_t;

char grid[H * W] = "##########"
                   "##########"
                   "##   #####"
                   "##    ####"
                   "#      ###"
                   "#        #"
                   "#       ##"
                   "##     ###"
                   "##########"
                   "##########";

static bool is_empty(vec2_t p) {
    return p.x >= 0 && p.x < W && p.y >= 0 && p.y < H &&
           grid[W * p.y + p.x] == EMPTY;
}

/*----------------------------------------------------------------------------*/
/* Flood fill */

static inline vec2_t queue_pop_front(vec2_t* q, int* top) {
    vec2_t ret = q[0];

    /* Shift. Note that `top` is not the last pushed value, but the next one */
    *top -= 1;
    for (int i = 0; i < *top; i++)
        q[i] = q[i + 1];

    return ret;
}

static inline void queue_push(vec2_t* q, int* top, vec2_t x) {
    q[*top] = x;
    *top += 1;
}

static void flood_fill(vec2_t p) {
    /* First in, first out */
    vec2_t* queue = malloc(W * H * sizeof(vec2_t));
    int queue_pos = 0;

    /* Push parameter and reveal it */
    queue_push(queue, &queue_pos, p);
    GRID_SET(p, REVEALED);

    /* Queue is not empty */
    while (queue_pos > 0) {
        const vec2_t cur = queue_pop_front(queue, &queue_pos);

        vec2_t up = { cur.x, cur.y - 1 };
        if (is_empty(up)) {
            queue_push(queue, &queue_pos, up);
            GRID_SET(up, REVEALED);
        }

        vec2_t down = { cur.x, cur.y + 1 };
        if (is_empty(down)) {
            queue_push(queue, &queue_pos, down);
            GRID_SET(down, REVEALED);
        }

        vec2_t left = { cur.x - 1, cur.y };
        if (is_empty(left)) {
            queue_push(queue, &queue_pos, left);
            GRID_SET(left, REVEALED);
        }

        vec2_t right = { cur.x + 1, cur.y };
        if (is_empty(right)) {
            queue_push(queue, &queue_pos, right);
            GRID_SET(right, REVEALED);
        }
    }
}

/*----------------------------------------------------------------------------*/

static void print_grid(void) {
    for (int y = 0; y < H; y++) {
        for (int x = 0; x < W; x++)
            putchar(grid[y * W + x]);
        putchar('\n');
    }
}

int main(void) {
    print_grid();
    putchar('\n');

    vec2_t start = { 5, 5 };
    flood_fill(start);

    print_grid();

    return 0;
}
