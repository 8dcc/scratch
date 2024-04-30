/* 1-10 */

#include <stdio.h>

main () {
	int c = 0;
	
	while ((c = getchar()) != EOF) {
        switch (c) {
            case '\t':
                printf("\\t");
                break;
            case '\b':
                printf("\\b");
                break;
            case '\\':
                printf("\\\\");
                break;
            default:
    		    putchar(c);
                break;
        }
	}
}
