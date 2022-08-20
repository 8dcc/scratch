#include <stdio.h>

main() {
	int c;  // Instead of char we read int to be able to understand EOF

	while ((c = getchar()) != EOF) {	// Will read and display (add to queue)
		putchar(c);						// the character until EOF
	}
}
