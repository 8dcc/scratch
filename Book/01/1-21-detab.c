/* 1-21 */

#include <stdio.h>

#define DEBUG_PRINT 1

#define TAB_SIZE 4		// The length of the file "columns"

int main() {
	char c;
	int line_count = 0, space_count = 0;	// Current line position and space count
	int line_pos = 0;

	int first_spaces, complete_tabs, last_spaces;

	while ( (c = getchar()) != EOF ) {
		if (c == '\n') {
			putchar(c);

			// Reset variables and add line count
			line_count++;
			space_count = 0;
			line_pos = 0;
		} else {
			// Every space, add count
			if (c == ' ') {
				space_count++;
			// Every non-space
			} else {
				// If there was a single space followed by a character, print space and clear variables
				if (space_count == 1) {
					putchar(' ');
				// We encounter a character but before that there was more than one space
				} else if (space_count > 1) {
					first_spaces = TAB_SIZE - (line_pos - space_count) % TAB_SIZE;	// Required spaces to get to the next tab
					//first_spaces = (first_spaces == 4) ? 0 : first_spaces;
					complete_tabs = (space_count - first_spaces) / TAB_SIZE;		// Full 4 space groups without the first and last ones
					last_spaces = space_count - first_spaces - complete_tabs * 4;

					if (space_count < first_spaces) {
						for (int n = 0; n < space_count; n++) {
							putchar(' ');
						}
					} else {
						if (first_spaces == 1) {
							putchar(' ');
						} else {
							putchar('\t');
						}

						for (int n = 0; n < complete_tabs; n++) {
							putchar('\t');
						}

						for (int n = 0; n < last_spaces; n++) {
							putchar(' ');
						}
					}

					if (DEBUG_PRINT == 0) {				
						printf("line_pos: %d\nspace_count: %d\n\nfs: %d\nct: %d\nls:%d\n",
								line_pos, space_count, first_spaces, complete_tabs, last_spaces);
						return 1;
					}
				}

				putchar(c);			// Print the non-space character 
				space_count = 0;
			}
			line_pos++;
		}
	}

	if (DEBUG_PRINT == 0) {
		printf("-------------------------\nReplaced %d lines.\n", line_count);
	}

	return 0;
}
