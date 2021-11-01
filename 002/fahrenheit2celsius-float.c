#include <stdio.h>

main(){
	float fahr, celsius;	// For more precise results
	int lower, upper, step;	// We cound make it float

	lower = 0;		// Lower limit of temp scale
	upper = 300;	// Higher limit of temp scale
	step = 10;		// Step size

	fahr = lower;
	while (fahr <= upper) {
		// Now we can multiply *5.0/9.0 because they are floats so c does not truncate
		celsius = (5.0/9.0) * (fahr-32.0);

		// Make a '4 digit + 0 decimals holder' and a '7 digit + 2 decimal holder'
		// Have in mind that the '7 digits' mean digits in total, including the dot
		printf("%4.0f  | %7.2f\n", fahr, celsius);

		fahr = fahr + step;
	}
}
