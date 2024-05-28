/* Kata: https://www.codewars.com/kata/5556282156230d0e5e000089 */

#include <stdlib.h>
#include <string.h>

char *dna_to_rna(const char *dna) {   
    // Did not work because I was pointing to dna, and trying to change a const
    char *rna = malloc(strlen(dna) + 1);
    strcpy(rna, dna);
    for (int n = 0; *(dna+n) != '\0'; n++) {
        if (dna[n] == 'T') rna[n] = 'U';
    }
    return rna;
}
