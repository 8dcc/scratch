/* 1-16 */

// Instead of using the same variable for the line lenght and the line array position,
// we use separate ones.

#include <stdio.h>

#define MAXLINE 20
#define PRINT_ALL 0

int get_line(char line[], int maxline);
void copy(char to[], char from[]);

int main() {
	int len, max;
	char line[MAXLINE], longest[MAXLINE];

	max = 0;
	while ( (len = get_line(line, MAXLINE)) > 0 ) {  // If the get_line returns a lenght longer than 0 (there are lines left)
		if (len > max) {
			max = len;  // Overwrite max if its bigger
			copy(longest, line);  // Copy the current line to longest array
		}
		
		// Print every line if PRINT_ALL
		if (PRINT_ALL == 0) {
			printf("%d - %s", len, line);
		}
	}

	if (max > 0) {  // There was at least a line
		printf("Longest: %s", longest);
	}
	return 0;
}

int get_line(char s[], int max_line_len) {
	int c;		// Char used by getchar()
	int i;		// Value to be returned (length). Will increase over time.
	int n = 0;	// Will count the array position (Instead of the length of the line)

	for (i = 0; (c = getchar()) != EOF && c != '\n'; i++) {
		if ( i < max_line_len-2 ) {		// For \n and \0
			s[n] = c;	// Store to the line array char by char
			n++;
		}
	}
	
	if (c == '\n') {
		s[n] = '\n';  // Add newline character to the line array
		n++;
		i++;
	}

	s[n] = '\0';  // Add null byte to the end of the array
	
	return i;  // Final length
}

void copy(char to[], char from[]) {
	int i = 0;

	while ((to[i] = from[i]) != '\0') {
		i++;
	}
}
