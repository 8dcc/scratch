/* 1-4 */

#include <stdio.h>

main(){
	int fahr, celsius;
	int lower, upper, step;

	lower = 0;		// Lower limit of temp scale
	upper = 300;	// Higher limit of temp scale
	step = 10;		// Step size

	celsius = lower;
	while (celsius <= upper) {
		// We don't multiply *9/5 because c truncates so we would multiply *1
		fahr = (celsius * 9) / 5 + 32;

		// Make a '3 digit holder' and a '6 digit holder' so it's right-justified
		printf("%3d %6d\n", celsius, fahr);

		celsius = celsius + step;
	}
}
