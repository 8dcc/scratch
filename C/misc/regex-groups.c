
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

static bool regex(const char* str, const char* pat, size_t* nmatch,
                  regmatch_t** pmatch) {
    static regex_t r;

    /*
     * Compile regex pattern ignoring case.
     */
    if (regcomp(&r, pat, REG_EXTENDED | REG_ICASE)) {
        fprintf(stderr,
                "regex: regcomp returned an error code for pattern \"%s\"\n",
                pat);
        return false;
    }

    /*
     * The size of the match array is the number of sub-expressions plus one
     * extra item for the entire match, which will be at index 0.
     */
    *nmatch = r.re_nsub + 1;
    *pmatch = malloc(*nmatch * sizeof(regmatch_t*));

    int code = regexec(&r, str, *nmatch, *pmatch, 0);
    regfree(&r);

    if (code != REG_NOERROR) {
        free(*pmatch);
        *nmatch = 0;
        *pmatch = NULL;

        /*
         * Unexpected error, in this case print it.
         */
        if (code != REG_NOMATCH) {
            char err[100];
            regerror(code, &r, err, sizeof(err));
            fprintf(stderr, "regex: regexec returned an error: %s\n", err);
        }

        return false;
    }

    return true;
}

static void print_match_data(const char* str, int base_offset, size_t nmatch,
                             regmatch_t* pmatch) {
    const int global_start = base_offset + pmatch[0].rm_so;
    const int global_end   = base_offset + pmatch[0].rm_eo;

    printf("Pattern match from %d to %d: \"", global_start, global_end);
    for (int i = global_start; i < global_end; i++)
        putchar(str[i]);
    printf("\"\n");

    /* Print each sub-expression */
    for (size_t i = 1; i < nmatch; i++) {
        if (pmatch[i].rm_so == -1 || pmatch[i].rm_eo == -1)
            break;

        const int start = base_offset + pmatch[i].rm_so;
        const int end   = base_offset + pmatch[i].rm_eo;

        printf("  Sub-expression %zu from %d to %d: \"", i, start, end);
        for (int j = start; j < end; j++)
            putchar(str[j]);
        printf("\"\n");
    }
}

int main(int argc, char** argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s \"PATTERN\" \"STRING\"\n", argv[0]);
        exit(1);
    }

    const char* pat = argv[1];
    const char* str = argv[2];

    int str_offset = 0;

    size_t nmatch;
    regmatch_t* pmatch;
    while (regex(&str[str_offset], pat, &nmatch, &pmatch)) {
        if (pmatch[0].rm_so == -1 || pmatch[0].rm_eo == -1)
            break;

        /* Print the global match and each sub-expression */
        print_match_data(str, str_offset, nmatch, pmatch);

        /*
         * Next iteration, continue searching from the end of the global match.
         */
        str_offset += pmatch[0].rm_eo;

        /*
         * Each (successful) call to `regex' requires the caller to free
         * `pmatch'.
         */
        free(pmatch);
    }

    return 0;
}
