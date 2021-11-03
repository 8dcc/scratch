#include <stdio.h>

// Exercise 1-6 and 1-7
main() {
	int c;

	printf("Input: ");
	while (1) {
		if ((c = getchar()) != EOF) {
			if (c == 10) {
				printf("c was not EOF, but was newline. EOF value: %d\n", EOF);	// 1-7
				printf("Input: ");
			} else {
				printf("c was not EOF, instead: %3d | ", c);	// Use %c for the char value (same as putchar())
				putchar(c);
				printf(" | %d\n", c != EOF);					// 1-6 (We use c instead of getchar() because that would execute it twice (at the start of the loop))
			}
		} else {
			printf("c was EOF! Value: %d\n", EOF);				// 1-7
			break;
		}
	}
}
