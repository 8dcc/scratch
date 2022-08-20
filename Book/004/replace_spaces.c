#include <stdio.h>

// 1-12
main () {
	int c = 0;
	
	while ((c = getchar()) != EOF) {
		if (c == ' ' || c == '\t') {
			c = '\n';
		}
		putchar(c);
	}
}
