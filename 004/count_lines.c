#include <stdio.h>

main () {
	int c = 0, nl = 0;
	
	while ((c = getchar()) != EOF) {
		if (c == '\n') {
			//printf("Detected new line. Value: %d\n", c);
			nl++;
		}
	}
	printf("Lines: %d\n", nl);
}
