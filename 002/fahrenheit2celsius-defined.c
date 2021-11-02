#include <stdio.h>

#define LOWER 0
#define UPPER 300
#define STEP 20

main() {
	int fahr;  // No need to use float

	for (fahr = LOWER; fahr <= UPPER; fahr = fahr + STEP) {
		printf("%4d  | %7.2f\n", fahr, (5.0/9.0)*(fahr-32));
	}
}
