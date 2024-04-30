
#include <stdio.h>

#define OFFSET 100

int main() {
    char filename[100];

    printf("Filename: ");
    scanf("%100s", filename);

    FILE* original_file = fopen(filename, "r");
    if (!original_file) {
        printf("Error. File does not exist.\n");
        return 1;
    }

    FILE* output_file = fopen("encoded.txt", "w");
    if (!output_file)
        return 1;

    char c;
    while ((c = fgetc(original_file)) != EOF) {
        c += OFFSET;
        fputc(c, output_file);
    }

    fclose(original_file);
    fclose(output_file);

    printf("Done. Check encoded.txt\n");

    return 0;
}
