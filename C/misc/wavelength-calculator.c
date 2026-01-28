/*
 * Copyright 2026 8dcc
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdio.h>

/*----------------------------------------------------------------------------*/

#define LIGHT_SPEED 299792458 /* meters per second */

/*
 * Return the wavelength (in meters) of the specified frequency (in Hertz).
 */
static inline double freq2wavelength(double freq) {
    return LIGHT_SPEED / freq;
}

/*
 * Return frequency (in Hertz) of the specified the wavelength (in meters).
 */
static inline double wavelength2freq(double wavelength) {
    return LIGHT_SPEED / wavelength;
}

/*----------------------------------------------------------------------------*/

#define LENGTH(ARR) (sizeof(ARR) / sizeof((ARR)[0]))

int main(void) {
    double freqs[] = {
        7000000, /* 7 MHz (40 meter band) */
        wavelength2freq(40),
        14000000, /* 14 MHz (20 meter band) */
        wavelength2freq(20),
        27000000, /* 27 MHz (CB) */
        wavelength2freq(11),
        29000000, /* 29 MHz (10 meter band) */
        wavelength2freq(10),
        145000000, /* 145 MHz (Repeaters) */
        wavelength2freq(2),
        wavelength2freq(0.70),
        446000000, /* 446 MHz (PMR) */
    };

    /* clang-format off */
    printf("| Freq (MHz) | Wavelength (m) | 1/2     | 1/4     | 1/8     | 1/16    | 1/32    |\n"
           "|------------+----------------+---------+---------+---------+---------+---------|\n");
    /* clang-format on */

    for (size_t i = 0; i < LENGTH(freqs); i++) {
        const double wavelength = freq2wavelength(freqs[i]);
        printf("| %10.2f | %14.4f | %7.4f | %7.4f | %7.4f | %7.4f | %7.4f |\n",
               freqs[i] / 1000000,
               wavelength,
               wavelength / 2,
               wavelength / 4,
               wavelength / 8,
               wavelength / 16,
               wavelength / 32);
    }

    return 0;
}
