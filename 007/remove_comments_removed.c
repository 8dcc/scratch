

#include <stdio.h>

#define DEBUG_PRINT 0	

int main() {
	int c;
	int line_count = 0;

	

	int danger = 0;

	while ((c = getchar()) != EOF) {
		if (c == '\n') {		
			
			if (danger == 2) {
				danger = 0;
			}

			if (danger != 3) {	
				putchar(c);		
			}

			line_count++;
		} else {				
			switch (c) {
				case '/':		
					if (danger == 0) {
						danger = 1;				
					} else if (danger == 1) {
						danger = 2;				
					} else if (danger == 4) {	
						danger = 0;
					}
					break;
				case '':
					if (danger == 1) {			
						danger = 3;				
					} else if (danger == 3) {	
						danger = 4;				
					}
					break;
				default:
					if (danger == 1) {			
						danger = 0;
						putchar('/');			
					} else if (danger == 4) {	
						danger = 3;
					}
					
					if (danger == 0) {
						putchar(c);
					}

					break;

			}  
		}  
	}  

	if (DEBUG_PRINT == 1) {
		printf("---------------------------------------------\nTotal lines: %d\n", line_count);
	}

	return 0;
}

