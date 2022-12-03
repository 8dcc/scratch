
#include <stdio.h>

static int count_s(char* s);
static int priority(char c);

int main() {
    int len     = 0;
    int total   = 0;
    int matched = 0;

    char buf[255] = { 0 };
    while (fgets(buf, 255, stdin)) {
        len = count_s(buf);

        for (int i = 0; i < len / 2; i++) {
            for (int j = len / 2; j < len; j++) {
                if (buf[i] == buf[j]) {
                    total += priority(buf[i]);

                    matched = 1;    // For breaking the outer for.
                    break;
                }
            }

            if (matched) {
                matched = 0;
                break;
            }
        }
    }

    printf("Total: %d\n", total);

    return 0;
}

// Length of s without null terminator
int count_s(char* s) {
    int i;

    for (i = 0; s[i] != '\0'; i++)
        ;

    return i;
}

int priority(char c) {
    if (c >= 'a' && c <= 'z')
        return c - 'a' + 1;
    else if (c >= 'A' && c <= 'Z')
        return c - 'A' + 27;
    else
        return 0;
}
