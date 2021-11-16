// 1.13 - Vertical graph
//		X - LENGHTS
//		Y - NUMBER OF MATCHES

#include <stdio.h>

#define MAXLEN 15		// Max word length. If the word is longer than this, will count as MAXLEN
#define MAXWORDS 25		// The max ammount of words the histogram will display
#define EMPTYCHAR ' '	// The char used to display empty characters in the graph
#define FULLCHAR '#'	// The char used to display full characters in the graph

main() {
	int c, wordcount = 0;
	int histogram[MAXLEN];

	// Clear the array
	for(int i = 0; i <= MAXLEN; i++) {
		histogram[i] = 0;
	}
	
	// Generate the array
	while ((c = getchar()) != EOF) {
		if(c == ' ' || c == '\n' || c == '\t') {
			histogram[wordcount]++;
			wordcount = 0;
		} else {
			if(wordcount < MAXLEN) {
				wordcount++;
			}
		}
	}

	// Display the graph
	for(int x = MAXWORDS-1; x >= 0; x--) {
		printf(" %2d|", x+1);	// Print the found number (first column of the output)
		for(int y = 1; y <= MAXLEN; y++) {
			// Check if the y pos (heigth) is more than the fount in the array
			if(histogram[y] > x) {
				printf("%c|", putchar(FULLCHAR));
			} else {
				printf("%c|", putchar(EMPTYCHAR));
			}
		}
		printf("\n");
	}

	// Last 2 lines
	printf("   |");
	for(int i = 1; i <= MAXLEN; i++) {
		printf("--|");		// Separator line
	}
	printf("\n");
	printf("   |"); // Last line
	for(int i = 1; i <= MAXLEN; i++) {
		printf("%2d|", i);	// Lengths
	}
	printf("\n");

}
