// 1-19

#include <stdio.h>

#define MAXLINE 200
#define PRINT_PREV 0

int get_line(char line[], int max_len);
void reverse_string(char string[]);

int main() {
	int len;  // Used to store the length of the line and check if its > 80
	char line[MAXLINE];  // We dont need longest

	while ( (len = get_line(line, MAXLINE)) > 0 ) {
		if (PRINT_PREV == 0) {
			printf("\n[%3d] %s", len, line);
		}

		reverse_string(line);
		printf("[%3d] %s", len, line);
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

// Reverses a string (ended by \0)
void reverse_string(char string[]) {

	// Get the length of the string
	int string_len = 0;
	while ( (string[string_len] != '\0') && (string[string_len] != '\n')) {
		string_len++;
	}

	// Reverse the string
	char aux_string[string_len];
	int aux_pos = 0;
	for (int n = string_len-1; n >= 0; n--) {
		aux_string[aux_pos] = string[n];
		aux_pos++;
	}

	// Overwrite the old string with the aux one
	for (int n = 0; n < string_len; n++) {
		string[n] = aux_string[n];
	}
}
