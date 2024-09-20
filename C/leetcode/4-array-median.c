
#include <stdio.h>
#include <stdlib.h>

/*
 * https://leetcode.com/problems/median-of-two-sorted-arrays/
 *
 * 4. Median of Two Sorted Arrays
 * ==============================
 *
 * Given two sorted arrays `nums1' and `nums2' of size `m' and `n' respectively,
 * return the median of the two sorted arrays.
 *
 * The overall run time complexity should be `O(log (m+n))'.
 *
 * Examples
 * --------
 *
 * - Input: nums1 = [1,3], nums2 = [2]
 * - Output: 2.00000
 * - Explanation: merged array = [1,2,3] and median is 2.
 *
 * - Input: nums1 = [1,2], nums2 = [3,4]
 * - Output: 2.50000
 * - Explanation: merged array = [1,2,3,4] and median is (2 + 3) / 2 = 2.5.
 *
 * Notes
 * -----
 *
 * This solution is simple but not ideal. It merges the two arrays and returns
 * the element in the middle.
 */

double findMedianSortedArrays(int* nums1, int nums1Size, int* nums2,
                              int nums2Size) {
    const int total_elements = nums1Size + nums2Size;

    /*
     * Merge the two arrays. Each iteration, store the smaller element in the
     * `merged' array, only increasing the pointer we stored.
     */
    int* merged = malloc(total_elements * sizeof(int));
    for (int i = 0, n1i = 0, n2i = 0; i < total_elements; i++) {
        if (n1i >= nums1Size) {
            if (n2i >= nums2Size)
                break; /* ??? */

            merged[i] = nums2[n2i++];
        } else if (n2i >= nums2Size) {
            merged[i] = nums1[n1i++];
        } else {
            merged[i] = (nums1[n1i] < nums2[n2i]) ? nums1[n1i++] : nums2[n2i++];
        }
    }

    const int median_idx = (total_elements - 1) / 2;
    double result        = merged[median_idx];

    /* Odd number of elements, average the two medians. */
    if (total_elements % 2 == 0) {
        result += merged[median_idx + 1];
        result /= 2.0;
    }

    free(merged);
    return result;
}

int main(void) {
    double result;

    int case1a[2] = { 1, 3 };
    int case1b[1] = { 2 };
    result        = findMedianSortedArrays(case1a, 2, case1b, 1);
    printf("Result 1: %f\n", result);

    int case2a[2] = { 1, 2 };
    int case2b[2] = { 3, 4 };
    result        = findMedianSortedArrays(case2a, 2, case2b, 2);
    printf("Result 2: %f\n", result);

    return 0;
}
