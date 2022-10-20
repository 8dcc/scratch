/* 3-03 */

#include <stdio.h>
#include <string.h>

static inline int isvalid(char c);
static char* expand(char* dest, const char* fmt, int dest_len);

int main() {
    char d[255] = { 0 };
    char f[255] = "a-z";

    expand(d, f, sizeof(d));
    printf("Format: %s\n"
           "-----------------------\n"
           "Expanded: %s\n\n",
           f, d);

    memset(d, '\0', sizeof(d));
    strcpy(f, "a-dA-G");
    expand(d, f, sizeof(d));
    printf("Format: %s\n"
           "-----------------------\n"
           "Expanded: %s\n\n",
           f, d);

    memset(d, '\0', sizeof(d));
    strcpy(f, "a-d\\-A-E-B");
    expand(d, f, sizeof(d));
    printf("Format: %s\n"
           "-----------------------\n"
           "Expanded: %s\n",
           f, d);

    return 0;
}

/*
 * Fills dest with the expanded format from fmt. Returns dest.
 *
 * Example:
 * "a-z": abcd...wxyz
 * "a-d-b": abcdcb
 * "a-d\-x-z": abcd-xyz (literally)
 */
char* expand(char* dest, const char* fmt, int dest_len) {
    char pc = 0;    // Next and previous chars when detecting a '-'
    char nc = 0;

    int dest_p = 0;    // Current char in the dest str

    // Scan for '-'. From second char to the penultimate.
    for (int i = 1; fmt[i] != '\0' && fmt[i + 1] != '\0'; i++) {
        // Only scan for character ranges
        if (fmt[i] != '-') continue;

        pc = fmt[i - 1];
        nc = fmt[i + 1];

        // Surrounded by valid chars
        if (isvalid(pc) && isvalid(nc)) {
            if (pc < nc) {
                // Ascending (abc)
                for (int j = pc; j <= nc && dest_p < dest_len; j++)
                    dest[dest_p++] = j;
            } else {
                // Descending (cba)
                for (int j = pc; j >= nc && dest_p < dest_len; j--)
                    dest[dest_p++] = j;
            }
        } else if (pc == '\\') {
            // The dash is escaped ("\-"). Add it literally.
            dest[dest_p++] = '-';
        }
    }

    return dest;
}

int isvalid(char c) {
    return ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z'));
}
