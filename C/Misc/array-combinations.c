
#include <stdio.h>

#define LENGTH(ARR) (int)(sizeof(ARR) / sizeof((ARR)[0]))

int main(void) {
    char arr[] = { 'a', 'b', 'c', 'd' };

    for (int i = 0; i < LENGTH(arr); i++)
        for (int j = i + 1; j < LENGTH(arr); j++)
            printf("%c - %c\n", arr[i], arr[j]);

    return 0;
}
