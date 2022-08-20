// 1-18

#include <stdio.h>

#define MAXLINE 200
#define PRINT_SPACES 0

int get_line(char line[], int maxline);
int remove_spaces(char string[], int string_len);

int main() {
	int len;  // Used to store the length of the line and check if its > 80
	int new_len;			// Returned value from remove_spaces()
	char line[MAXLINE];		// We dont need longest

	while ( (len = get_line(line, MAXLINE)) > 0 ) {
		if (PRINT_SPACES == 0) {
			printf("\n[%3d] %s", len, line);
		}

		new_len = remove_spaces(line, len);
		if (new_len > 1) {
			printf("[%3d] %s", new_len, line);
		}
	}

	return 0;
}

// Same as 1-16
int get_line(char s[], int max_line_len) {
	int c;		// Char used by getchar()
	int i;		// Value to be returned (length). Will increase over time.
	int n = 0;	// Will count the array position (Instead of the length of the line)

	for (i = 0; (c = getchar()) != EOF && c != '\n'; i++) {
		if ( i < max_line_len-2 ) {
			s[n] = c;
			n++;
		}
	}
	
	if (c == '\n') {
		s[n] = '\n';
		n++;
		i++;
	}

	s[n] = '\0';
	
	return i;
}

// Remove spaces from string and return new len
int remove_spaces(char s[], int string_len) {
	char s_aux[string_len];
	int space_warning = 0;  // 0 = safe
	int j, i = 0;
	
	for (int n = 0; n < string_len; n++) {
		// Check excesive spaces
		if (s[n] == ' ') {
			if (space_warning == 0) {
				space_warning = 1;
			} else { // 1 or 2
				space_warning = 2;
			}
		} else {
			space_warning = 0;
		}

		// Check safe characters to write in the aux array
		if (s[n] != '\t' && space_warning != 2) {
			s_aux[i] = s[n];
			i++;
		}
	}

	// If the line was not empty, write to main array
	if (i > 1) {
		// Clear array
		for (int n = 0; n < string_len; n++) {
			s[n] = 0;
		}
		
		// Write s_aux to s
		for (j = 0; j < i; j++) {
			s[j] = s_aux[j];
		}
		s[j+1] = '\n';
		s[j+2] = '\0';
	}

	return i;
}

