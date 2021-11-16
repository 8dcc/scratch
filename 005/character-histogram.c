// 1.14 - Only lowercase letters

#include <stdio.h>

#define DICTLEN		27		// a-z
#define MAXCOUNT 	50		// The max ammount of words the histogram will display

main() {
	int c, other = 0;
	int histogram[DICTLEN];

	// Clear the array
	for(int i = 0; i <= DICTLEN; i++) {
		histogram[i] = 0;
	}

	while ((c = getchar()) != EOF) {
		// Detect whitespaces
		if(c >= 'a' && c <= 'z') {
			if(histogram[c-'a'] <= MAXCOUNT) {
				histogram[c-'a']++;
			}
		} else {
			other++;
		}
	}

	// Loop through each histogram item (-2 because it starts at 0 and <=)
	for(int i = 0; i <= DICTLEN-2; i++) {
		printf("Character %c: ", 'a'+i);
		for(int n = 0; n < histogram[i] && n < MAXCOUNT; n++) {
			printf("-");					// Print a "-" for each character count.
		}
		for(int x = histogram[i]; x <= MAXCOUNT; x++) {
			printf(" ");					// Fill the remaining spaces
		}
		printf(" %2d\n", histogram[i]);		// Print the number at the end
	}
	printf("Other: %d\n", other);
}
