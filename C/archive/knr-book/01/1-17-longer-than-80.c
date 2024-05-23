/* 1-17 */

#include <stdio.h>

#define MAXLINE 200
#define PRINT_ALL 1

int get_line(char line[], int maxline);

int main() {
	int len;  // Used to store the length of the line and check if its > 80
	char line[MAXLINE];  // We dont need longest

	while ( (len = get_line(line, MAXLINE)) > 0 ) {
		if ( (len > 80 || PRINT_ALL == 0) && (len > 0) ) {
			printf("[%3d] %s", len, line);
		}
	}

	return 0;
}

// Same as 1-16
int get_line(char s[], int max_line_len) {
	int c;		// Char used by getchar()
	int i;		// Value to be returned (length). Will increase over time.
	int n = 0;	// Will count the array position (Instead of the length of the line)

	for (i = 0; (c = getchar()) != EOF && c != '\n'; i++) {
		if ( i < max_line_len-2 ) {
			s[n] = c;
			n++;
		}
	}
	
	if (c == '\n') {
		s[n] = '\n';
		n++;
		i++;
	}

	s[n] = '\0';
	
	return i;
}

