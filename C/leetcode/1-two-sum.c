
#include <stdio.h>
#include <stdlib.h>

/*
 * https://leetcode.com/problems/two-sum/
 *
 * 1. Two Sum
 * ============================================================================
 *
 * Given an array of integers `nums' and an integer `target', return indices of
 * the two numbers such that they add up to `target'.
 *
 * You may assume that each input would have exactly one solution, and you may
 * not use the same element twice.
 *
 * You can return the answer in any order.
 *
 * Example
 * ----------------------------------------------------------------------------
 *
 * - Input: nums = [2,7,11,15], target = 9
 * - Output: [0,1]
 * - Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].
 */

int* twoSum(int* nums, int numsSize, int target, int* returnSize) {
    *returnSize = 2;
    int* result = malloc(2 * sizeof(int));

    for (int i = 0; i < numsSize; i++) {
        for (int j = i + 1; j < numsSize; j++) {
            if (nums[i] + nums[j] == target) {
                result[0] = i;
                result[1] = j;
                goto done;
            }
        }
    }

done:
    return result;
}

int main(void) {
    int unused;
    int* result;

    int case1[4] = { 2, 7, 11, 15 };
    result       = twoSum(case1, 4, 9, &unused);
    printf("Result 1: %d, %d\n", result[0], result[1]);
    free(result);

    int case2[3] = { 3, 2, 4 };
    result       = twoSum(case2, 3, 6, &unused);
    printf("Result 2: %d, %d\n", result[0], result[1]);
    free(result);

    int case3[2] = { 3, 3 };
    result       = twoSum(case3, 2, 6, &unused);
    printf("Result 3: %d, %d\n", result[0], result[1]);
    free(result);

    return 0;
}
