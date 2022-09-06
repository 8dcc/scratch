/* Book misc - 10 */

#include <stdio.h>

main() {
	int array1[10];
	int array2[10];

	printf("Array 1: [ ");
	for(int i = 0; i <= 10; i++){
		array1[i] = i;
		printf("%2d ", array1[i]);
	}
	
	printf("]\nArray 2: [ ");
	for(int n = 10; n >= 0; n--){
		array2[n] = n;
		printf("%2d ", array2[n]);
	}
	printf("]");
}
