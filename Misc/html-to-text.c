
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

/* string.h function does weird stuff when strings overlap (our case) */
static char* my_strcpy(char* dest, const char* src) {
    size_t i;

    for (i = 0; src[i] != '\0'; i++)
        dest[i] = src[i];

    dest[i] = '\0';

    return dest;
}

static char* html2txt(char* str) {
    char* ret = str;

    bool print_newline = false;
    bool in_label      = false;
    char* label_start  = str;

    while (*str != '\0') {
        switch (*str) {
            case '<':
                in_label    = true;
                label_start = str;
                str++;
                break;
            case '>':
                if (print_newline) {
                    *label_start = '\n';
                    label_start++;
                }

                in_label = false;
                my_strcpy(label_start, str + 1);
                str = label_start;
                break;
            default:
                if (in_label) {
                    if (*str++ == 'b' && *str++ == 'r')
                        print_newline = true;
                } else {
                    str++;
                }

                break;
        }
    }

    return ret;
}

int main() {
    char str[] = "<a href=\"#p96207730\" "
                 "class=\"quotelink\">96207730</a><br>Let me rephrase: would "
                 "it be possible for an individual, or small group of "
                 "individuals, to do this.";

    printf("Original:\n%s\n\n", str);
    printf("Final:\n%s\n", html2txt(str));

    return 0;
}
