/* 1-13 */

#include <stdio.h>

#define MAXLEN 20		// Max word length. If the word is longer than this, will count as MAXLEN
#define MAXWORDS 20		// The max ammount of words the histogram will display

main() {
	int c, wordcount = 0;
	int histogram[MAXLEN];

	// Clear the array
	for(int i = 0; i <= MAXLEN; i++) {
		histogram[i] = 0;
	}

	while ((c = getchar()) != EOF) {
		// Detect whitespaces
		if(c == ' ' || c == '\n' || c == '\t') {
			histogram[wordcount]++;
			wordcount = 0;
		} else {
			if(wordcount < MAXLEN) {
				wordcount++;
			}
		}
	}

	// Loop through each histogram item
	for(int i = 1; i <= MAXLEN; i++) {
		printf("Length %2d: ", i);
		for(int n = 0; n < histogram[i] && n < MAXWORDS; n++) {
			printf("-");					// Print a "-" for each word count.
		}
		for(int x = histogram[i]; x <= MAXWORDS; x++) {
			printf(" ");					// Fill the remaining spaces
		}
		printf(" %2d\n", histogram[i]);		// Print the number at the end
	}
}
