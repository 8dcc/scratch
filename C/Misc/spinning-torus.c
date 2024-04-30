
#include <stdio.h>
#include <unistd.h> /* usleep */
#include <math.h>   /* M_PI, cosf, sinf */

#define CONSOLE_CLEAR "\x1B[H"

#define SCR_W 50
#define SCR_H 30

#define THETA_SPACING (0.07f)
#define PHI_SPACING   (0.02f)

#define R1 (1.0f)
#define R2 (2.0f)
#define K2 (5.0f)
#define K1 (SCR_H * K2 * 3 / (8 * (R1 + R2)))

static void render_frame(float A, float B) {
    static const char light_chars[] = ".,-~:;=!*#$@";

    static char output[SCR_H][SCR_W];
    static char zbuffer[SCR_H][SCR_W];

    for (int y = 0; y < SCR_H; y++) {
        for (int x = 0; x < SCR_W; x++) {
            output[y][x]  = ' ';
            zbuffer[y][x] = 0;
        }
    }

    float cosA = cosf(A), sinA = sinf(A);
    float cosB = cosf(B), sinB = sinf(B);

    for (float theta = 0; theta < 2 * M_PI; theta += THETA_SPACING) {
        float costheta = cosf(theta);
        float sintheta = sinf(theta);

        for (float phi = 0; phi < 2 * M_PI; phi += PHI_SPACING) {
            float cosphi = cosf(phi);
            float sinphi = sinf(phi);

            float circlex = R2 + R1 * costheta;
            float circley = R1 * sintheta;

            float x = circlex * (cosB * cosphi + sinA * sinB * sinphi) -
                      circley * cosA * sinB;
            float y = circlex * (sinB * cosphi - sinA * cosB * sinphi) +
                      circley * cosA * cosB;
            float z   = K2 + cosA * circlex * sinphi + circley * sinA;
            float ooz = 1.f / z;

            int xp = (int)(SCR_W / 2 + K1 * ooz * x);
            int yp = (int)(SCR_H / 2 - K1 * ooz * y);

            if (xp < 0 || xp >= SCR_W || yp < 0 || yp >= SCR_H)
                continue;

            float L = cosphi * costheta * sinB - cosA * costheta * sinphi -
                      sinA * sintheta +
                      cosB * (cosA * sintheta - costheta * sinA * sinphi);

            if (L > 0.f && ooz > zbuffer[yp][xp]) {
                zbuffer[yp][xp]     = ooz;
                int luminance_index = L * 8;
                output[yp][xp]      = light_chars[luminance_index];
            }
        }
    }

    printf("%s", CONSOLE_CLEAR);
    for (int y = 0; y < SCR_H; y++) {
        for (int x = 0; x < SCR_W; x++)
            putchar(output[y][x]);
        putchar('\n');
    }
}

int main(void) {
    for (float a = 0.f, b = 0.f;; a += 0.04f, b += 0.02f) {
        render_frame(a, b);
        usleep(50000);
    }

    return 0;
}
