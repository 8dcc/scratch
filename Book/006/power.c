#include <stdio.h>

// Function prototype (m and n are not necesary)
int power(int m, int n);

main() {
	for(int i = 0; i < 10; i++) {
		printf("Testing  2^%d: %6d\nTesting -3^%d: %6d\n", i, power(2, i), i, power(-3, i));
	}
	return 0;
}

int power(int base, int n) {
	// Start multiplying base by 1 = base
	// Will skip the loop if n is <= 0, returning 1
	int p = 1;

	// Multiply base by itself n times
	for(int i = 1; i <= n; i++) {
		p = p * base;
	}
	return p;
}
