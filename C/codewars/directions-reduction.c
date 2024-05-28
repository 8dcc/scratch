/* Kata: https://www.codewars.com/kata/550f22f4d758534c1100025a */
/* Not completed */

#include <string.h>

void del_arr_item(char** arr, int size, int pos);

char** dirReduc(char** arr, int sz, int* lg) {
    int recalculate = 1;
    *lg = sz;

    while (recalculate) {
        recalculate = 0;        // Timeouts
        for (int n = 1; n < sz; n++) {
            if (strcmp(arr[n], "NORTH") && strcmp(arr[n-1], "SOUTH")) {
                del_arr_item(arr, sz, n);
                del_arr_item(arr, sz, n-1);
                *lg -= 2;
                recalculate = 1;
            } else if (strcmp(arr[n], "SOUTH") && strcmp(arr[n-1], "NORTH")) {
                del_arr_item(arr, sz, n);
                del_arr_item(arr, sz, n-1);
                *lg -= 2;
                recalculate = 1;
            } else if (strcmp(arr[n], "WEST") && strcmp(arr[n-1], "EAST")) {
                del_arr_item(arr, sz, n);
                del_arr_item(arr, sz, n-1);
                *lg -= 2;
                recalculate = 1;
            } else if (strcmp(arr[n], "EAST") && strcmp(arr[n-1], "WEST")) {
                del_arr_item(arr, sz, n);
                del_arr_item(arr, sz, n-1);
                *lg -= 2;
                recalculate = 1;
            }
        }
    }

    return arr;
}



// check whether the deletion is possible or not  
void del_arr_item(char** arr, int size, int pos) {
    if (pos <= size) {  
        for (int n = pos - 1; n < size -1; n++) {  
            arr[n] = arr[n+1];  
        }
    }
}

