/* 1-6 */

#include <stdio.h>

main() {
    int r;  // For checking if the check is 1 or 0
	int c;  // Instead of char we read int to be able to understand EOF

	c = getchar();		// Will get the first character
	while (r = (c != EOF)) {	// Until we press enter (EOF)
        printf("%d", r);        // Print numerical value of r (check)

		c = getchar();	// Get the next one
	}

    printf("%d", r);    // Print r when we exit the loop as well
}
