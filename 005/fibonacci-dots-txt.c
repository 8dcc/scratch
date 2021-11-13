// Usage:
// fibonacci-dots-txt.out > fib-output.txt
// Where 20 is the number of steps

#include <stdio.h>

#define STEPS 20		// The number of steps
#define CHARACTER '#'	// The character it will print. Make sure you use ' ', not " "

main() {
	int c;
	int fib[100] = {1, 1};

	for (int empty = 2; empty < 100; empty++){
		fib[empty] = 0;
	}

	for (int n = 2; n < STEPS; n++){
		fib[n] = fib[n-1] + fib[n-2];
	}

	for (int n = 0; n < STEPS; n++) {
		for (int dots = 0; dots < fib[n]; dots++) {
			putchar(CHARACTER);
		}
		putchar('\n');
	}
}
