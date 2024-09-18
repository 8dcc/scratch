
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

int main(void) {
    const char* str = "static int func(int num0, char c, int num1)";
    const char* pat = "(int)";

    int base_idx = 0;

    size_t nmatch;
    regmatch_t* pmatch;
    while (regex(&str[base_idx], pat, &nmatch, &pmatch)) {
        if (pmatch[1].rm_so == -1 || pmatch[1].rm_eo == -1)
            break;

        int start_idx = base_idx + pmatch[1].rm_so;
        int end_idx   = base_idx + pmatch[1].rm_eo;

        printf("Match from %d to %d: \"", start_idx, end_idx);
        for (int i = start_idx; i < end_idx; i++)
            putchar(str[i]);
        printf("\"\n");

        /*
         * Next iteration, continue searching from the end of the last match.
         */
        base_idx = end_idx;

        /*
         * Each (successful) call to `regex' requires the caller to free
         * `pmatch'.
         */
        free(pmatch);
    }

    return 0;
}
