#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define ARRAY_LEN 100
#define ITEM_LEN 3      // Hex num + '\0'
#define SLIDER_LEN 7

void init_arr(char* arr, size_t strlen);
void print_full_arr(char* arr, size_t strlen);
void print_arr(char* arr, size_t strlen, int start, int len);
void process_input(int* option);

enum {
    OPT_NONE = 0,
    OPT_ADD,
    OPT_SUB,
    OPT_QUIT
};

int main() {
    char arr[ITEM_LEN][ARRAY_LEN];     // Array with len 100 containing 3 char strings
    init_arr(&arr[0][0], ITEM_LEN);
    print_full_arr(&arr[0][0], ITEM_LEN);

    int slider_count = 0;               // Current 'page' of the array (imagine you are pressing page down)
    int option = OPT_NONE;

    while (1) {
        process_input(&option);

        switch (option) {
            case OPT_ADD:
                // The 'page' we are tring to show is not greater than the 'page' number
                if (slider_count < ARRAY_LEN / SLIDER_LEN) slider_count++;
                break;
            case OPT_SUB:
                if (slider_count > 0) slider_count--;
                break;
            case OPT_QUIT:
                return 0;
            default:
                // OPT_NONE does not do anything
                break;
        }

        printf("[%d/%d] ", slider_count, ARRAY_LEN/SLIDER_LEN);
        print_arr(&arr[0][0], ITEM_LEN, slider_count * SLIDER_LEN, SLIDER_LEN);
    }

    return 0;
}

void init_arr(char* arr, size_t strlen) {
    for (int n = 0; n < ARRAY_LEN; n++) {
        char* cur_pos = &arr[n*strlen];     // Get pointer to start of new string

        sprintf(cur_pos, "%x", n);          // Save n in hex format
    }
}

void print_full_arr(char* arr, size_t strlen) {
    printf("Full array:\n");
    for (int n = 0; n < ARRAY_LEN; n++) {
        // Create variables for readability. Real pos is the pos inside the 1d array.
        int real_pos = n * strlen;

        printf("%s ", &arr[real_pos]);
    }
    putchar('\n');
}

void print_arr(char* arr, size_t strlen, int start, int len) {
    // Iterate numbers from 0 to SLIDER_LEN (7)
    for (int n = 0; n < len; n++) {
        // n is the display position inside this 'page' and arr_pos is the item position of the item in the array
        int arr_pos = n + start;
        // real_pos is the real byte position inside arr (converting 2d arr to 1d)
        int real_pos = arr_pos * strlen;

        // Break if we are trying to print out of bounds
        if (arr_pos >= ARRAY_LEN) break;

        printf("%2s ", &arr[real_pos]);
    }
    putchar('\n');
}

void process_input(int* opt) {
    int c;

    while (1) {
        printf("\nWhat do you want to do? (n/p) > ");
        c = getchar();

        switch (tolower(c)) {
            case '\n':      return;     // If newline, dont change opt (aka repeat last input) and dont getchar again
            case 'n':       *opt = OPT_ADD;     getchar();      return;
            case 'p':       *opt = OPT_SUB;     getchar();      return;
            case 'q':       *opt = OPT_QUIT;    getchar();      return;
            default:        printf("Wrong option.\n");          break;  // Ask again
        }
        getchar();      // Clear newline after input
    }
}
