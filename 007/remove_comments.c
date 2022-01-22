// 1-23

#include <stdio.h>

#define DEBUG_PRINT 0	// 1 = ON

int main() {
	int c;
	int line_count = 0;

	/*
	 * The danger variable determines if we encountered a '/' (1), so
	 * the program knows that it needs to check if the next character
	 * is another '/' (2), a '*' (3 or 4 depending on the previous state)
	 * or other (0).
	 */

	int danger = 0;

	while ((c = getchar()) != EOF) {
		if (c == '\n') {		// Every line
			// If there was a '//' in the line, the comment ends when the line ends
			if (danger == 2) {
				danger = 0;
			}

			if (danger != 3) {	// We are not in a multiline comment
				putchar(c);		// Put the newline
			}

			line_count++;
		} else {				// Every character of the line
			switch (c) {
				case '/':		// Start of comment is always '/'
					if (danger == 0) {
						danger = 1;				// We might start a comment
					} else if (danger == 1) {
						danger = 2;				// We are in a singleline comment
					} else if (danger == 4) {	// There was a * before and we are in a multiline comment
						danger = 0;
					}
					break;
				case '*':
					if (danger == 1) {			// There was a '/' before
						danger = 3;				// We are in a multiline comment
					} else if (danger == 3) {	// If we are in a multiline comment,
						danger = 4;				// check if we are closing it
					}
					break;
				default:
					if (danger == 1) {			// If the '/' is not followed by comment reset danger
						danger = 0;
						putchar('/');			// Put the missing '/'
					} else if (danger == 4) {	// If the * in a multiline comment was not followed by '/'
						danger = 3;
					}
					
					if (danger == 0) {
						putchar(c);
					}

					break;

			}  // end of c switch
		}  // end of \n check
	}  // end of while loop

	if (DEBUG_PRINT == 1) {
		printf("---------------------------------------------\nTotal lines: %d\n", line_count);
	}

	return 0;
}

