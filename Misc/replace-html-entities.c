
#include <stdio.h>
#include <string.h>

#define LENGTH(ARR) (int)(sizeof(ARR) / sizeof((ARR)[0]))

typedef struct {
    int strlen;
    const char* str;
    char c;
} HtmlEntityPair;

static char* replace_html_entities(char* str) {
    static const HtmlEntityPair ents[] = {
        { 6, "&quot;", '\"' }, /**/
        { 6, "&apos;", '\'' }, /**/
        { 5, "&amp;", '&' },   /**/
        { 4, "&lt;", '<' },    /**/
        { 4, "&gt;", '>' },    /**/
    };

    /* Each HTML entity */
    for (int i = 0; i < LENGTH(ents); i++) {
        char* cur_ent;

        /* Each ocurrence in the target string */
        while ((cur_ent = strstr(str, ents[i].str)) != NULL) {
            /* Replace with real char */
            *cur_ent = ents[i].c;

            /* Shift the rest of the string */
            strcpy(cur_ent + 1, cur_ent + ents[i].strlen);
        }
    }

    return str;
}

int main() {
    char str[] = "Hello &quot; &apos; &amp; &lt; &gt; world!";

    printf("%s\n", replace_html_entities(str));

    return 0;
}
