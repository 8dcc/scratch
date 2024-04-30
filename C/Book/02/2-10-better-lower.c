/* 2-10 */

#include <stdio.h>

#define BUFF_SIZE 255

int lower(int character);

int main() {
    char str[BUFF_SIZE] = "I AM a teSt for TEstiNg thE lowerCase sTuff...";
    char lower_str[BUFF_SIZE];

    for (int n = 0; n < BUFF_SIZE && str[n] != '\0'; n++)
        lower_str[n] = lower(str[n]);

    printf("Unchanged variable: %s\n", str);
    printf("Lowercase variable: %s\n", lower_str);

    return 0;
}

int lower(int character) {
    return (character >= 'A' && character <= 'Z') ? character + 'a' - 'A'
                                                  : character;
}
