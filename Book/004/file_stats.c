#include <stdio.h>

// Exercise 1-8
main () {
	int c = 0, lines = 0, spaces = 0, tabs = 0, other = 0;
	
	while ((c = getchar()) != EOF) {
		// Could use ifs but I am not like yandere dev or some shit. Even I know that lmao
		switch (c) {
			case '\n' :
				lines++;
				break;
			case ' ' :
				spaces++;
				break;
			case '\t' :
				tabs++;
				break;
			default :
				other++; // Why not
		}
	}
	printf("Lines: %d | Spaces: %d | Tabs: %d | Other: %d\n", lines, spaces, tabs, other);
}
