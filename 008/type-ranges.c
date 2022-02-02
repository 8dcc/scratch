// 2-1

#include <stdio.h>
#include <limits.h>

int main() {
	printf("Char: %d | %d | %d\n", CHAR_MIN, CHAR_BIT, CHAR_MAX);
	printf("Unsigned Char: %d\n", UCHAR_MAX);
	printf("Int: %d | %d\n", INT_MIN, INT_MAX);
	printf("Unsigned Int: %d\n", UINT_MAX);
	printf("Long: %d | %d\n", LONG_MIN, LONG_MAX);
	printf("Unsigned Long: %d\n", ULONG_MAX);
	printf("Short: %d | %d\n", SHRT_MIN, SHRT_MAX);
	printf("Unsigned Short: %d\n", USHRT_MAX);

	return 0;
}
