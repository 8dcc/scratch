#include <stdio.h>

/* Comment ok shut up */
// Another comment shut the fuck up
main(){
	int fahr, celsius;
	int lower, upper, step;

	lower = 0;		// Lower limit of temp scale
	upper = 300;	// Higher limit of temp scale
	step = 10;		// Step size

	fahr = lower;
	while (fahr <= upper) {
		// We don't multiply *5/9 because c truncates so we would multiply *0
		celsius = 5 * (fahr-32) / 9;

		// Make a '3 digit holder' and a '6 digit holder' so it's right-justified
		printf("%3d %6d\n", fahr, celsius);

		fahr = fahr + step;
	}
}
