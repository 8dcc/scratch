
#include <stdint.h>
#include <stdio.h>
#include <string.h> /* strlen() */
#include <stdlib.h> /* malloc(), free() */

static const uint8_t mem[] = "ABCDEFGHIJK";

/* Just to illustrate this example. Could be replaced with `process_vm_readv'.
 *
 * NOTE: Try printing the read bytes inside this function if you have trouble
 * understanding how the buffering works. */
void* mem_read(void* dst, const void* src, size_t sz) {
    uint8_t* out      = dst;
    const uint8_t* in = src;

    while (sz-- > 0)
        *out++ = *in++;

    return out;
}

/* Search for the byte sequence `pat' from the address `start' to the address
 * `end', while reading the memory in chunks of size `strlen(pat)'.
 *
 * The `end' address is NOT read.
 *
 * The pattern is a null-terminated byte array, and it's assumed to be greater
 * than the search region (end-start). */
void* buffered_search(uintptr_t start, uintptr_t end, const char* pat) {
    /* The input will be buffered, and the pattern will be searched in here.
     *
     * NOTE: This is the minimum buffer size, you can make it bigger to save
     * calls to `mem_read'. */
    int buf_sz   = strlen(pat);
    uint8_t* buf = malloc(buf_sz);
    mem_read(buf, (void*)start, buf_sz);

    /* Real address that buf[0] is mapped to. Will increase whenever the buffer
     * is repopulated. */
    uintptr_t chunk_start = start;

    /* Current position inside the pattern */
    int pat_pos = 0;

    /* Current position inside the buffer. If it reaches `buf_sz', the buffer
     * will be repopulated. */
    int buf_pos = 0;

    /* Position inside the buffer where the current match started */
    int match_start = 0;

    /* While we have more memory and pattern left to read */
    while ((chunk_start + buf_pos) < end && pat[pat_pos] != '\0') {
        /* Check if we reached the end of the buffer. If so, repopulate it. */
        if (buf_pos >= buf_sz) {
            if (match_start == buf_pos) {
                /* If we are not inside a match (because we are already at the
                 * start of the match):
                 *
                 * Increase the address that we are mapping (move to the next
                 * chunk of size `buf_sz'). */
                chunk_start += buf_sz;

                /* And reset the position inside the buffer and the pattern to
                 * zero. */
                buf_pos = 0;
                pat_pos = 0;
            } else {
                /* Otherwise, if we are not inside a match:
                 *
                 * Repopulate from the start of the match, and don't change the
                 * position inside the pattern. */
                chunk_start += match_start;

                /* We matched N characters of the input, and the buffer is now
                 * mapped to the match start. The current position in the new
                 * buffer is the position we were on the (partial) match. */
                buf_pos = pat_pos;
            }

            /* The match start is always going to be zero on repopulation, even
             * if we were in a match already. */
            match_start = 0;

            /* Make sure we aren't reading past the `end' address */
            if (chunk_start + buf_sz > end)
                buf_sz = end - chunk_start;

            /* Repopulate the buffer with the new chunk */
            mem_read(buf, (void*)chunk_start, buf_sz);
        }

        if (buf[buf_pos] == pat[pat_pos]) {
            /* We found an exact byte match with the pattern. Go to next byte in
             * the buffer. */
            buf_pos++;
            pat_pos++;
        } else {
            /* A byte didn't match, increase the match start position and search
             * again from there. */
            match_start++;
            buf_pos = match_start;
            pat_pos = 0;
        }
    }

    /* If we reached end of the pattern, return the match. Otherwise, NULL. */
    void* ret =
      (pat[pat_pos] == '\0') ? (void*)(chunk_start + match_start) : NULL;

    free(buf);

    return ret;
}

int main(void) {
    uintptr_t start = (uintptr_t)(mem);
    uintptr_t end   = (uintptr_t)(mem + sizeof(mem) - 1);
    const char* pat = "FGHI";

    void* result = buffered_search(start, end, pat);

    printf("Start:  %p\n", (void*)start);
    printf("End:    %p\n", (void*)end);
    printf("Result: %p\n\n", result);

    printf("Data:   %s\n", mem);

    if (!result)
        return 1;

    printf("Result: ");
    const int match_pos = (uintptr_t)result - start;
    for (int i = 0; i < match_pos; i++)
        putchar(' ');
    printf("^ (+%d)\n", match_pos);

    return 0;
}
