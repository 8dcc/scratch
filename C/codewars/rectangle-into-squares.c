/*
 * Kata: https://www.codewars.com/kata/55466989aeecab5aac00003e
 * Not complete. Needs to get the size of `arr` and get the max ammount
 * of squares, not the min.
 */

#include <stdlib.h>

typedef struct Data Data;
struct Data {
    int* array;
    int sz;
};

Data* sqInRect(int ol, int ow) {
    Data* data_ptr = malloc(sizeof(Data));
    if (ol == ow) {                 // Original w and l is a square
        data_ptr->array = NULL;
        data_ptr->sz = 0;           // Return size 0 acoring to instructions
        return data_ptr;
    };

    int l = ol, w = ow;
    int arr[255], sz = 0;           // For the values. The problem is getting the arr size
    while(w != l) {                 // Until we reach the smallest square
        if (w < l) {
            arr[sz++] = w;
            l -= w;
        } else {
            arr[sz++] = l;
            w -= l;
        }
    }
    arr[sz] = w;                    // Last value

    data_ptr->sz = sz;
    data_ptr->array = arr;
    return data_ptr;
}
