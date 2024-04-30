
#include <stdio.h>

static int priority(char c);

int main() {
    int total   = 0;
    int matched = 0;

    char buf1[255] = { 0 };
    char buf2[255] = { 0 };
    char buf3[255] = { 0 };
    while (fgets(buf1, 255, stdin) && fgets(buf2, 255, stdin) &&
           fgets(buf3, 255, stdin)) {
        for (int i = 0; buf1[i] != '\n'; i++) {
            for (int j = 0; buf2[j] != '\n'; j++) {
                // If a char matches on the first 2, check the 3rd one
                if (buf1[i] == buf2[j]) {
                    for (int k = 0; buf3[k] != '\n'; k++) {
                        if (buf1[i] == buf3[k]) {
                            total += priority(buf1[i]);

                            matched = 1;    // For breaking the outer for.
                            break;
                        }
                    }
                }

                // Don't set to 0 yet
                if (matched)
                    break;
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

int priority(char c) {
    if (c >= 'a' && c <= 'z')
        return c - 'a' + 1;
    else if (c >= 'A' && c <= 'Z')
        return c - 'A' + 27;
    else
        return 0;
}
