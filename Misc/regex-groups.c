#include <stdbool.h>
#include <stdio.h>
#include <regex.h>

static bool regex(const char* str, const char* pat, size_t nmatch,
                  regmatch_t* pmatch) {
    static regex_t r;

    /* Compile regex pattern ignoring case */
    if (regcomp(&r, pat, REG_EXTENDED | REG_ICASE)) {
        fprintf(stderr,
                "regex: regcomp returned an error code for pattern \"%s\"\n",
                pat);
        return false;
    }

    int code = regexec(&r, str, nmatch, pmatch, 0);
    if (code > REG_NOMATCH) {
        char err[100];
        regerror(code, &r, err, sizeof(err));
        fprintf(stderr, "regex: regexec returned an error: %s\n", err);
        return false;
    }

    /* REG_NOERROR: Success
     * REG_NOMATCH: Pattern did not match */
    return code == REG_NOERROR;
}

int main(void) {
    const char* str = "static int func(int num0, char c, int num1)";
    const char* pat = "(int)";

    size_t nmatch = 2;
    regmatch_t pmatch[2];

    int str_idx = 0;

    bool result = regex(str, pat, nmatch, pmatch);
    while (result) {
        int real_start = str_idx + pmatch[1].rm_so;
        int real_end   = str_idx + pmatch[1].rm_eo;

        printf("Found from %d to %d: \"", real_start, real_end);

        for (int i = real_start; i < real_end; i++)
            putchar(str[i]);

        printf("\"\n");

        str_idx = real_end;
        result  = regex(&str[str_idx], pat, nmatch, pmatch);
    }

    return 0;
}
