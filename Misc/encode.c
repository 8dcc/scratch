// https://github.com/r4v10l1

#include<stdio.h>

#define OFFSET 100

int main() {
    char filename[100], c;
    FILE *original_file, *output_file;

    printf("Filename: ");
	scanf("%100s", &filename);

	// Open output file as read
    original_file = fopen(filename, "r");
    if(original_file == NULL) {
		printf("Error. File does not exist.\n");
        return 1;
	}

	// Open output file as write
    output_file = fopen("encoded.txt", "w");
    if(output_file == NULL) {	
        return 1;
	}

	// Get each character of the original file until EOF
    while((c = fgetc(original_file)) != EOF) {
        c = c + OFFSET;			// We add the offset to enconde our file
        fputc(c, output_file);	// We write the new char to the output file
    }

	// Close the files
    fclose(original_file);
    fclose(output_file);

	printf("Done. Check encoded.txt\n");
    
	return 0;
}
