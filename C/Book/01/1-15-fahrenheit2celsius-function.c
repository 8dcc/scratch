/* 1-15 */

#include <stdio.h>

float fahr2cel(int celsius);

main() {
	for (int n = 0; n <= 300; n = n+20){
		printf("Testing %3d: %6.2f\n", n, fahr2cel(n));
	}
	return 0;
}

float fahr2cel(int f) {
	return (5.0/9.0)*(f-32);
}
