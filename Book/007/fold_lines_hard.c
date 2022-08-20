// 1-22 Bad

#include <stdio.h>

#define MAX_LINE_SIZE 255
#define MAX_OUTPUT_SIZE 20

int main() {
	int c;
	int line_pos = 0;

	char line[MAX_LINE_SIZE + 1];	// For the null character

	// Clear the array
	for (int n = 0; n < MAX_LINE_SIZE + 1; n++) {
		line[n] = '\0';
	}

	// Read the input and store everything in the array
	while ((c = getchar()) != EOF && c != '\n') {
		if (line_pos >= MAX_LINE_SIZE) {
			printf("Your line is too long!\n");
			return 1;
		}

		line[line_pos] = c;
		line_pos++;
	}
	line[line_pos] = '\0';	// Should not be necesary since we cleared the array

	// Read the array and wrap the lines
	int line_size = line_pos;
	for (line_pos = 0; line_pos < line_size; line_pos++) {
		if (line_pos != 0 && line_pos % MAX_OUTPUT_SIZE == 0) {
			putchar('\n');
		}

		putchar(line[line_pos]);
	}

	return 0;
}
