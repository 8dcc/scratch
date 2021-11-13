#include <stdio.h>

main() {
	int c, i = 0;
	int fib[100] = {1, 1};

	// Empty the rest of the array
	for (int empty = 2; empty < 100; empty++){
		fib[empty] = 0;
	}

	printf("Number of steps: ");
	while((c = getchar()) != EOF){
		if (c == '\n') {
			break;
		}
		// We multiply *10 to shift the previous digit to the left.
		// We subtract 48 to get the int value from the char.
		i = (i * 10) + (c - 48);
	}

	// Thing itself
	for (int n = 2; n < i; n++){
		fib[n] = fib[n-1] + fib[n-2];
	}

	// Print the array (we could do it in the same for loop)
	for (int n = 0; n < i; n++) {
		printf("%d ", fib[n]);
	}
}
