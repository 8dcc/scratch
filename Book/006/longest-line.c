#include <stdio.h>

#define MAXLINE 1000

int get_line(char line[], int maxline);
void copy(char to[], char from[]);

int main() {
	int len, max;
	char line[MAXLINE], longest[MAXLINE];

	max = 0;
	// getchar() will be called in the get_line function
	while ( (len = get_line(line, MAXLINE)) > 0 ) {  // If the get_line returns a lenght longer than 0 (there are lines left)
		if (len > max) {
			max = len;  // Overwrite max if its bigger
			copy(longest, line);  // Copy the current line to longest array
		}
	}

	if (max > 0) {  // There was at least a line
		printf("%s", longest);
	}
	return 0;
}

int get_line(char s[], int max_line_len) {
	int c;  // Char used by getchar()
	int i;  // Value to be returned (length). Will increase over time.

	// Loop unless the line ends (EOF or newline).
	// We define i outside the loop so we can return it after the loop ends.
	for (i = 0; i < max_line_len-1 && (c = getchar()) != EOF && c != '\n'; i++) {
		s[i] = c;  // Store to the line array char by char
	}

	// If the line ended in \n instead of EOF
	if (c == '\n') {
		s[i] = c;  // Add newline character to the line array
		i++;
	}

	s[i] = '\0';  // Add null byte to the end of the array
	return i;  // Final length
}

// We don't need to return a value because we can write to arrays outside the function.
void copy(char to[], char from[]) {
	int i = 0;

	// Set the value of "to[i]" to "from[i]" in the loop itself
	while ((to[i] = from[i]) != '\0') {
		i++;
	}
}
