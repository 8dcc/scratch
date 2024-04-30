/* 1-12 */

#include <stdio.h>

main () {
	int c = 0;
	
	while ((c = getchar()) != EOF) {
		if (c == ' ' || c == '\t') {
			c = '\n';
		}
		putchar(c);
	}
}
