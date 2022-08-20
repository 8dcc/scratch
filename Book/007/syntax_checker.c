/*
 * 1-24
 * It will check for comments, and ignore them,
 * unmached parentheses, brackets, quotes, etc.
 * On a single line!
 *
 * TODO: Make it check for more than one parentheses for example:
 * dick(123, 'a', (cum))		-- OK
 * dick(123, 'a', (cum)			-- NOT OK
 */

#include <stdio.h>

#define DEBUG_PRINT 1	// 1 = ON
#define BUFFER_SIZE 1000
#define TAB_SIZE 8

void clear_buffer(char buffer[]);
void print_syntax_error(char line[], int error_pos, int tab_count);

int main() {
	int c;
	char line_array[BUFFER_SIZE];
	int line_count = 0, character_position = 0, tab_count = 0;
	/*
	 * If danger is 0, we are not in a comment so we must check the line for errors
	 * 0 - We good
	 * 1 - There was a / so we have to be careful
	 * 2 - We are in a single line comment
	 * 3 - We are in a multiline comment
	 * 4 - There was a * in a multiline comment
	 */
	int danger = 0;
	int parentheses_checker = -1, brackets_checker = -1, braces_checker = -1,
		s_quote_checker = -1, d_quote_checker = -1;

	// Clear the array first
	clear_buffer(line_array);

	while ((c = getchar()) != EOF) {
		if (c == '\n') {
			// For comment checking
			if (danger == 2) {
				danger = 0;
			}

			// Add the newline to the buffer
			line_array[character_position] = '\n';

			// If there was an error, print the line and the error
			if (parentheses_checker != -1) {
				print_syntax_error(line_array, parentheses_checker, tab_count);
			} else if (brackets_checker != -1) {
				print_syntax_error(line_array, brackets_checker, tab_count);
			} else if (braces_checker != -1) {
				print_syntax_error(line_array, braces_checker, tab_count);
			} else if (s_quote_checker != -1) {
				print_syntax_error(line_array, s_quote_checker, tab_count);
			} else if (d_quote_checker != -1) {
				print_syntax_error(line_array, d_quote_checker, tab_count);
			}

			// Clear the checker variables each line
			parentheses_checker = -1;
			brackets_checker = -1;
			braces_checker = -1;
			s_quote_checker = -1;
			d_quote_checker = -1;

			// Clear character position, tab count and the buffer
			character_position = 0, tab_count = 0;
			line_count++;
			clear_buffer(line_array);
		} else {
			// Check for comments
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
				case '*':
					if (danger == 1) {
						danger = 3;
					} else if (danger == 3) {
						danger = 4;
					}
					break;
				default:
					if (danger == 1) {
						danger = 0;
					} else if (danger == 4) {
						danger = 3;
					}

					break;
			}

			// Check for errors in the syntax
			if (danger == 0) {
				switch (c) {
					case '(':  // See todo
						if (parentheses_checker == -1) {
							parentheses_checker = character_position;
						}
						break;
					case ')':
						if (parentheses_checker == -1) {
							parentheses_checker = character_position;
						} else {
							parentheses_checker = -1;
						}
						break;
					case '[':
						if (brackets_checker == -1) {
							brackets_checker = character_position;
						}
						break;
					case ']':
						if (brackets_checker == -1) {
							brackets_checker = character_position;
						} else {
							brackets_checker = -1;
						}
						break;
					case '{':
						if (braces_checker == -1) {
							braces_checker = character_position;
						}
						break;
					case '}':
						if (braces_checker == -1) {
							braces_checker = character_position;
						} else {
							braces_checker = -1;
						}
						break;
					case '\'':
						if (s_quote_checker == -1) {
							s_quote_checker = character_position;
						} else {
							s_quote_checker = -1;
						}
						break;
					case '\"':
						if (d_quote_checker == -1) {
							d_quote_checker = character_position;
						} else {
							d_quote_checker = -1;
						}
						break;
					default:
						break;
				}  // end of syntax switch
			}  // end of danger check
		}  // end of \n check

		// For print_syntax_error
		if (c == '\t' && tab_count == character_position) {
			tab_count++;
		}

		if (c != '\n') {
			line_array[character_position] = c;
			character_position++;
		}
	}  // end of while loop

	if (DEBUG_PRINT == 1) {
		printf("---------------------------------------------\nTotal lines: %d\n", line_count);
	}

	return 0;
}


void clear_buffer(char buffer[]) {
	for (int n = 0; n < BUFFER_SIZE; n++) {
		buffer[n] = 0;
	}
}

void print_syntax_error(char line[], int error_pos, int tab_count) {
	printf("There was an error in the following line:\n%s", line);
	
	// Print the tab size
	for (int t = 0; t < tab_count; t++) {
		for (int ts = 0; ts < TAB_SIZE; ts++) {
			printf(" ");
		}
	}

	// Print the spaces before the signal
	for (int n = 0; n < error_pos - tab_count; n++) {
		printf(" ");
	}

	// Print the signal and 2 newlines
	printf("^\n\n");
}
