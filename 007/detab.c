// 1-20

#include <stdio.h>

#define TABSTOP_LEN 4  // The length of the file "columns"

int main() {
	char c;
	int tab_pos = 4, line_count = 0;  // Current line and character position
	int spaces_to_write;

	while ( (c = getchar()) != EOF ) {
		if (c == '\n') {  // Every line
			putchar(c);

			line_count++;
			tab_pos = 4;  // Reset character position
		} else {  // Every character
			if (c == '\t') {
				spaces_to_write = tab_pos % TABSTOP_LEN;
				spaces_to_write = 4 - spaces_to_write;

				for (int n = 0; n < spaces_to_write; n++) {
					putchar('-');
				}
			} else {
				putchar(c);  // Fuck printf
			}

			tab_pos++;
		}
	}

	printf("-------------------------\nReplaced %d lines.\n", line_count);
	return 0;
}
