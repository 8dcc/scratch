/* 1-9 */

#include <stdio.h>

main () {
	int c = 0, last_c = 0;
	
	while ((c = getchar()) != EOF) {
        // If we are trying to print more than one space, ignore
        if (c == ' ' && last_c == ' ')
            continue;

        last_c = c;
		putchar(c);
	}
}
