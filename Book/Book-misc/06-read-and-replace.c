/* Book misc - 06 */
// 1-9 and 1-10 exercises but in a confusing way

#include <stdio.h>

main () {
	int c = 0, space_count = 0;
	
	while ((c = getchar()) != EOF) {
		if (c == ' ') {		// 1-9
			if (space_count != 0) {
				c = 0;
			} else {
				space_count = 1;
			}
		} else {
			space_count = 0;
		}

		if (c == '\t') {
			putchar('\\');
			c = 't';
		} else if (c == '\b') {
			putchar('\\');
			c = 'b';
		} else if (c == '\\') {
			c = '\\';
			putchar(c);		// We just execute putchar() one more time
		}
		putchar(c);
	}
}
