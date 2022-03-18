// 2-2

#include <stdio.h>

int main() {
	char c;
	int n;
	int lim = 10;

	for (n = 0; n < lim - 1; n++) {
		if ( (c = getchar()) != '\n' ) {
			if (c != EOF) {
				s[n] = c;
			}
		}
	}
}
