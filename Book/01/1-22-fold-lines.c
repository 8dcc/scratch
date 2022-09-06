/* 1-22 */

#include <stdio.h>

#define MAX_LINE_SIZE 500
#define MAX_OUTPUT_SIZE 40
#define MAX_OUTPUT_LINES 20

int main() {
	int c;
	int line_pos = 0, subline_pos = 0;
	int max_space_pos = 0, extra_chars = 0, newline_count = 0;

	int newline_positions[MAX_OUTPUT_LINES];
	char line[MAX_LINE_SIZE + 1];	// For the null character

	// Clear the arrays
	for (int n = 0; n < MAX_LINE_SIZE + 1; n++) {
		line[n] = '\0';
	}
	for (int n = 0; n < MAX_OUTPUT_LINES; n++) {
		newline_positions[n] = 0;
	}

	// Read the input and store everything in the array
	while ((c = getchar()) != EOF && c != '\n') {
		if (line_pos >= MAX_LINE_SIZE) {
			printf("Your line is too long!\n");
			return 1;
		}

		// Store the last space position
		if (c == ' ') {
			max_space_pos = line_pos;
		}

		// When the subline ends
		if (line_pos != 0 && subline_pos >= MAX_OUTPUT_SIZE - extra_chars) {
			newline_positions[newline_count] = max_space_pos;
			newline_count++;

			extra_chars = line_pos - max_space_pos;
			subline_pos = 0;
		}

		line[line_pos] = c;
		line_pos++;
		subline_pos++;
	}
	line[line_pos] = '\0';	// Should not be necesary since we cleared the array


	// Read the array and wrap the lines
	int line_size = line_pos;
	int can_i_print = 1;
	for (line_pos = 0; line_pos < line_size; line_pos++) {
		for (int n = 0; n < MAX_OUTPUT_LINES; n++) {
			if (newline_positions[n] != 0 && line_pos == newline_positions[n]) {
				putchar('\n');
				can_i_print = 0;
				break;
			}
		}

		if (can_i_print == 1) {
			putchar(line[line_pos]);
		}

		can_i_print = 1;
	}

	return 0;
}
