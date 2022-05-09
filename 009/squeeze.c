// 2-4

#include <stdio.h>

void squeeze(char* str, const char* chars2remove);

int main() {
    // Tests
    char chars_to_remove[] = "AEIOUaeiou";
    char str1[] = "He likes the sea.";
    char str2[] = "skrrt - 1337";
    char str3[] = "";

    printf("Replacement: %s | Original: %17s ", chars_to_remove, str1);
    squeeze(str1, chars_to_remove);
    printf("| Replaced: %s\n", str1);

    printf("Replacement: %s | Original: %17s ", chars_to_remove, str2);
    squeeze(str2, chars_to_remove);
    printf("| Replaced: %s\n", str2);
    
    printf("Replacement: %s | Original: %17s ", chars_to_remove, str3);
    squeeze(str3, chars_to_remove);
    printf("| Replaced: %s\n", str3);

    return 0;
}

void squeeze(char* str, const char* r_str) {
    int np = 0;         // New string pos. Will contain the real position of the shifted string
    int can_write = 1;  // For knowing if we should write this char

    for (int op = 0; str[op] != '\0'; op++) {          // Every char of str
        for (int rp = 0; r_str[rp] != '\0'; rp++) {    // Loop each r_str to see if we can't use this char
            if (str[op] == r_str[rp]) {
                can_write = 0;                      // So we don't write this char
                break;
            }
        }
        if (can_write) str[np++] = str[op];          // Write to new pos and increase value if the char is not in r_str
        else can_write = 1;                         // If it was in r_str, reset value
    }
    str[np] = '\0';
}
