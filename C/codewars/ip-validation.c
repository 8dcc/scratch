/* Kata: https://www.codewars.com/kata/515decfd9dcfc23bb6000006 */

#include <string.h>

int valid_fields(const char*);

int is_valid_ip(const char *addr) {
    if (!valid_fields(addr)) return 0;                  // Too few or many dots
    if (addr[0] == '0' || addr[0] == '.') return 0;     // Starts with 0 or .

    int this_num = 0;
    for (unsigned long n = 0; n < strlen(addr); n++) {
        if (n > 0 && addr[n-1] == '.') {    // Start of a number part
            if (addr[n] == '0') return 0;   // Leading zeros
        } else if (addr[n] == '.') {
            if (this_num < 0 || this_num > 255) return 0; // A number is too big or small
            this_num = 0;
        }

        if (addr[n] != '.') {                               // Not the cleanest way
            if (addr[n] < '0' || addr[n] > '9') return 0;   // Not a number
            this_num = this_num * 10 + (addr[n] - '0');     // Count the current number part
        }
    }

    return 1;
};

/* ----- Functions ----- */

int valid_fields(const char *addr) {
    int r = 0;
    for (unsigned long n = 0; n < strlen(addr); n++) {
        if (addr[n] == '.') r++;
    }
    return (r == 3);
}
