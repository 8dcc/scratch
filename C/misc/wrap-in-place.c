
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h> /* isspace */

/*
 * Wrap long lines of the specified string in-place. If a character exceeds the
 * 'max_column' (non-inclusive), a newline is inserted in the previous space of
 * that line, if there is one.
 *
 * The original input pointer is returned.
 */
char* wrap_in_place(char* str, unsigned int max_column) {
    char* const original_str = str;

    size_t last_newline_idx = 0;
    size_t last_space_idx   = 0;
    for (size_t i = 0; str[i] != '\0'; i++) {
        if (isspace(str[i])) {
            last_space_idx = i;
            if (str[i] == '\n')
                last_newline_idx = i;
        }

        /*
         * If we were about to exceed the maximum column and we stored a space
         * index since we changed into this line, overwrite the last space with
         * a newline.
         *
         * NOTE: This assumes that all characters use the same width, which is
         * not normally the case. A simple hack for this would be to count tabs
         * and set 'cur_line_width' to something like:
         *
         *     (i - last_newline_idx) + (tab_count * (tab_width - 1))
         */
        const size_t cur_line_width = i - last_newline_idx;
        if (cur_line_width > max_column) {
            str[last_space_idx] = '\n';
            last_newline_idx    = last_space_idx;
        }
    }

    return original_str;
}

/*----------------------------------------------------------------------------*/

void test_wrap(char* str, int max_column) {
    for (int i = 0; i < max_column; i++)
        putchar('-');
    putchar('\n');
    printf("%s", wrap_in_place(str, max_column));
    for (int i = 0; i < max_column; i++)
        putchar('-');
    putchar('\n');
}

int main(void) {
    static const char original_str[] =
      "Hello, world!\n\n"
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod "
      "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim "
      "veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea "
      "commodo consequat. Duis aute irure dolor in reprehenderit in voluptate "
      "velit esse cillum dolore eux fugiat nulla pariatur. Excepteur sint "
      "occaecat cupidatat non proident, sunt in culpa qui officia deserunt "
      "mollit anim id est laborum.\n"
      "Aenean nec odio cursus, pellentesque felis nec, maximus felis. In "
      "efficitur interdum nisl at auctor. Praesent fringilla, tortor id "
      "scelerisque cursus, quam quam vehicula nisi, non posuere dolor dolor at "
      "orci. Mauris in risus in purus sollicitudin pharetra. Cras lorem justo, "
      "iaculis in rhoncus faucibus, vulputate in nibh. Nunc imperdiet faucibus "
      "interdum. Aliquam turpis nibh, auctor in tellus sed, eleifend accumsan "
      "nulla. Pellentesque a arcu fermentum, imperdiet est vel, tincidunt "
      "sapien. Nam porta diam a ante volutpat efficitur. Praesent accumsan est "
      "purus, a ultricies ante varius nec. Mauris a elit a nisl dictum "
      "posueres sed vitae neque. Curabitur eu convallis nunc, id vestibulum "
      "nunc. Integer faucibus mollis tortor eget placerat.\n\n"
      "Bye.\n";
    char copy_str[sizeof(original_str)];

    strncpy(copy_str, original_str, sizeof(copy_str));
    test_wrap(copy_str, 80);

    strncpy(copy_str, original_str, sizeof(copy_str));
    test_wrap(copy_str, 72);

    return 0;
}
