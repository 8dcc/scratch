
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

/* Number of items in the array */
#define ARR_SZ 100

/* Size of each page when printing */
#define PAGE_SZ 7

typedef enum EOption {
    OPT_INVALID,
    OPT_PREV,
    OPT_NEXT,
    OPT_QUIT,
} EOption;

/*----------------------------------------------------------------------------*/

static void arr_init(int* arr) {
    for (int i = 0; i < ARR_SZ; i++)
        arr[i] = i;
}

static void arr_print_all(int* arr) {
    for (int i = 0; i < ARR_SZ; i++)
        printf("%02X ", arr[i] % 0xFF);

    putchar('\n');
}

static void arr_print(int* arr, int page) {
    /* Start and end indexes in the array, with pagination */
    const int start = page * PAGE_SZ;
    const int end   = (page + 1) * PAGE_SZ;

    for (int i = start; i < end && i < ARR_SZ; i++)
        printf("%02X ", arr[i] % 0xFF);

    putchar('\n');
}

static inline void input_empty(void) {
    /* Input is buffered until newline, skip rest of the chars that the user
     * input. */
    while (getchar() != '\n')
        ;
}

static EOption input_process(void) {
    for (;;) {
        printf("\nWhat do you want to do? [p/n]: ");
        int c = getchar();

        switch (tolower(c)) {
            case 'p':
                input_empty();
                return OPT_PREV;
            case 'n':
                input_empty();
                return OPT_NEXT;
            case 'q':
                input_empty();
                return OPT_QUIT;
            default:
                printf("Wrong option.\n");
                break;
        }
    }
}

/*----------------------------------------------------------------------------*/

int main() {
    int arr[ARR_SZ];
    arr_init(arr);

    printf("Printing array without pagination:\n");
    arr_print_all(arr);

    /* Current page in the array */
    int page = 0;

    /* Last page of the array */
    const int last_page = ((ARR_SZ - 1) / PAGE_SZ);

    for (;;) {
        EOption option = input_process();

        switch (option) {
            case OPT_PREV:
                if (page > 0)
                    page--;
                break;
            case OPT_NEXT:
                if (page < last_page)
                    page++;
                break;
            case OPT_QUIT:
                return 0;
            case OPT_INVALID:
                break;
        }

        /* Print with pagination */
        printf("[%d/%d] ", page, last_page);
        arr_print(arr, page);
    }

    return 0;
}
