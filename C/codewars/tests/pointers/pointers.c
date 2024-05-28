/*
 * Thanks to:
 * http://www.praxagora.com/doc_model/understanding_c_pointers_1.0.html#arrays 
 */

#include <stdio.h>
#include <string.h>

void clear_char_array(char[], int);
char *dna_to_rna(const char *dna);
char *dna_to_rna_alt(const char *dna);

int main() {
    char array_test[255];
    clear_char_array(array_test, 255);
    char *p;

    array_test[0] = 'a';
    array_test[1] = 'B';
    p = array_test;
    array_test[2] = '7';
    printf("%s\n", p);

    printf("[");
    for (int n = 0; n < 5; n++) {
        printf("%c ", *(p+n));
    }
    printf("]\n");

    printf("[");
    for (int n = 0; n < 5; n++) {
        printf("%c ", p[n]);
    }
    printf("]\n");

    char *str_test = "ABC UU 123";
    printf("%s", dna_to_rna(str_test));
    printf("%s", dna_to_rna_alt(str_test));

    int number = 123;
    int *nump;
    nump = &number;
    printf("\n%d", *nump);

    return 0;
}

void clear_char_array(char arr[], int size) {
    for (int n = 0; n < size; n++) {
        arr[n] = '\0';
    }
}

char *dna_to_rna(const char *dna)
{
    char *rna = dna;
    printf("%c\n", *(dna+0));
    for (int n = 0; n < 255 && *(dna+n) != '\0'; n++) {
        printf("%d | %c | %c\n", n, *(rna+n), *(rna+n));
        if (*(dna+n) == 'T') *(rna+n) = 'U';
        // The else fails, but not needed :(
        /* else *(rna+n) = *(dna+n); */
    }
    return rna;
}

char *dna_to_rna_alt(const char *dna)
{
    char *rna = dna;
    printf("%c\n", *(dna+0));
    for (int n = 0; n < 255 && *(dna+n) != '\0'; n++) {
        printf("%d | %c | %c\n", n, rna[n], rna[n]);
        if (dna[n] == 'T') rna[n] = 'U';
    }
    return rna;
}
