/*
 * Search for the byte sequence `pat' from the address `start' to the address
 * `end', while reading the memory in chunks of size `strlen(pat)'.
 *
 * The `end' address is NOT read. The pattern is a null-terminated byte array,
 * and it's assumed to be greater than the search region (end minus start).
 *
 * The general process of the buffered search is the following:
 *
 * 1. Iterate while we haven't reached the `end' address, and while the pattern
 *    isn't fully matched.
 * 2. Check if we have reached the end of our buffer. If we have, we need to
 *    repopulate it with new data.
 * 2.1. If we are not inside a match, just increase the address that our buffer
 *      is mapped to and reset the buffer and pattern positions. If we were in a
 *      match, we want to repopulate where the match started, and we want to
 *      continue from that position.
 * 2.2. In either case, we want to reset the match start position inside the
 *      buffer to zero.
 * 2.3. We need to check if the size we are trying to read actually reads past
 *      the `end' address. If so, only read up to that point.
 * 2.4. Actually read the data using some kind of memcpy() function.
 * 3. Compare the byte at the current position inside our buffer with the
 *    current pattern character.
 * 3.1. If it matches, just move to the next byte in the buffer and the pattern,
 *      without modifying where the match started.
 * 3.2. If it didn't match, increase the position where the potential match will
 *      start, moving it to the next position in the buffer. Also reset the
 *      position inside the pattern.
 *
 * Once we are done, we can know if we matched the pattern or not by checking
 * how we exited the loop. If we read the whole pattern, we found a match.
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h> /* strlen() */
#include <stdlib.h> /* malloc(), free() */

/* Sample data */
static const uint8_t mem[] = "ABCDEFGHIJK";

void* mem_read(void* dst, const void* src, size_t sz) {
    /* Just to illustrate this example. Could be replaced with
     * `process_vm_readv'.
     *
     * NOTE: Try printing the read bytes inside this function if you have
     * trouble understanding how the buffering works. */
    return memcpy(dst, src, sz);
}

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

    while ((chunk_start + buf_pos) < end && pat[pat_pos] != '\0') {
        if (buf_pos >= buf_sz) {
            if (match_start == buf_pos) {
                chunk_start += buf_sz;
                buf_pos = 0;
                pat_pos = 0;
            } else {
                chunk_start += match_start;
                buf_pos = pat_pos;
            }

            match_start = 0;

            if (chunk_start + buf_sz > end)
                buf_sz = end - chunk_start;

            mem_read(buf, (void*)chunk_start, buf_sz);
        }

        if (buf[buf_pos] == pat[pat_pos]) {
            buf_pos++;
            pat_pos++;
        } else {
            match_start++;
            buf_pos = match_start;
            pat_pos = 0;
        }
    }

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
