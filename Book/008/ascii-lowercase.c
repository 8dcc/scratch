#include <stdio.h>

#define ARRAY_SIZE 255

int lower(int character);
int upper(int character);
int sentence(char textin[], char textout[], int len);

int main() {
    char string_variable[] = "I AM a teSt for TEstiNg thE lowerCase sTuff...";
    char lower_variable[ARRAY_SIZE], upper_variable[ARRAY_SIZE], sentence_variable[ARRAY_SIZE];

    for (int n = 0; n < ARRAY_SIZE || string_variable[n] != '\0'; n++) {
        lower_variable[n] = lower(string_variable[n]);
        upper_variable[n] = upper(string_variable[n]);
    }
    sentence(string_variable, sentence_variable, ARRAY_SIZE);

    printf("Unchanged variable: %s\n", string_variable);
    printf("Lowercase variable: %s\n", lower_variable);
    printf("Uppercase variable: %s\n", upper_variable);
    printf("Sentence variable:  %s\n", sentence_variable);
    return 0;
}

int lower(int character) {
    if (character >= 'A' && character <= 'Z') return character + 'a' - 'A';
    else return character;
}

int upper(int character) {
    if (character >= 'a' && character <= 'z') return character + 'A' - 'a';
    else return character;
}

int sentence(char textin[], char textout[], int len) {
    int first_char = 1;

    for (int n = 0; n < len && textin[n] != '\0'; n++) {
        if (first_char && textin[n] >= 'a' && textin[n] <= 'z') {
            textout[n] = upper(textin[n]);
            first_char = 0;
        } else {
            textout[n] = lower(textin[n]);
        }
    }

    return 0;
}
