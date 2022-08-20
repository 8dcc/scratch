#include <stdio.h>

main() {
	int c;  // Instead of char we read int to be able to understand EOF

	c = getchar();		// Will get the first character
	while (c != EOF) {	// Until we press enter (EOF)
		putchar(c);		// Print that character (Wait to be printed on the next line)
		c = getchar();	// Get the next one
	}
}
