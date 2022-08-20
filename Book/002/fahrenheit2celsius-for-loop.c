#include <stdio.h>

main(){
	 // We don't use any float fahr values, we only need to tell printf that the formula is a float
	int fahr;

	/*   lower     upper          step             */
	for (fahr = 0; fahr <= 300; fahr = fahr + 20) {
		printf("%4d  | %7.2f\n", fahr, (5.0/9.0) * (fahr-32));  // Apply formula in the same line
	}
}
