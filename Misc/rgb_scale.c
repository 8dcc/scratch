#include <stdint.h> /* uint8_t */
#include <stdio.h>  /* printf */

#define MIN(a, b) ((a) > (b) ? b : a)

typedef struct {
    uint8_t r, g, b, a;
} rgba_t;

rgba_t col_scale(rgba_t c, float factor) {
    if (factor < 1.0f)
        return (rgba_t){
            .r = (uint8_t)((float)c.r * factor),
            .g = (uint8_t)((float)c.g * factor),
            .b = (uint8_t)((float)c.b * factor),
            .a = c.a,
        };
    else if (factor > 1.0f)
        return (rgba_t){
            .r = (uint8_t)MIN(255, c.r + (float)(255 - c.r) * (factor - 1.f)),
            .g = (uint8_t)MIN(255, c.g + (float)(255 - c.g) * (factor - 1.f)),
            .b = (uint8_t)MIN(255, c.b + (float)(255 - c.b) * (factor - 1.f)),
            .a = c.a,
        };
    else
        return c;
}

#define PRINT_COLOR(c) \
    printf("%18s: (%3d, %3d, %3d, %3d)\n", #c, c.r, c.g, c.b, c.a);

int main() {
    rgba_t c1 = {
        .r = 67,
        .g = 160,
        .b = 71,
        .a = 255,
    };

    PRINT_COLOR(c1);                 /* https://color.hailpixel.com/#43A047 */
    PRINT_COLOR(col_scale(c1, 1.3)); /* https://color.hailpixel.com/#7BBC7E */
    PRINT_COLOR(col_scale(c1, 0.5)); /* https://color.hailpixel.com/#215023 */

    rgba_t c2 = {
        .r = 3,
        .g = 155,
        .b = 229,
        .a = 255,
    };

    PRINT_COLOR(c2);                 /* https://color.hailpixel.com/#039BE5 */
    PRINT_COLOR(col_scale(c2, 1.3)); /* https://color.hailpixel.com/#4EB9EC */
    PRINT_COLOR(col_scale(c2, 0.5)); /* https://color.hailpixel.com/#014D72 */

    return 0;
}
