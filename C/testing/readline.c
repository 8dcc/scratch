
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

int main(void) {
    puts("Type \"quit\" to exit.");

    bool quit = false;
    while (!quit) {
        char* input = readline("Input: ");

        /* The readline() function detected EOF with an empty line */
        if (input == NULL)
            break;

        add_history(input);

        if (!strcmp(input, "quit"))
            quit = true;
        else
            printf("Unknown command: \"%s\"\n", input);

        /* Free allocated memory from readline() */
        free(input);
    }

    return 0;
}
