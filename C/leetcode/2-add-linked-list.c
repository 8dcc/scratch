
#include <stdio.h>
#include <stdlib.h>

/*
 * https://leetcode.com/problems/add-two-numbers/
 *
 * 2. Add Two Numbers
 * ==================
 *
 * You are given two non-empty linked lists representing two non-negative
 * integers. The digits are stored in reverse order, and each of their nodes
 * contains a single digit. Add the two numbers and return the sum as a linked
 * list.
 *
 * You may assume the two numbers do not contain any leading zero, except the
 * number 0 itself.
 *
 * Example
 * -------
 *
 * - Input: l1 = [2,4,3], l2 = [5,6,4]
 * - Output: [7,0,8]
 * - Explanation: 342 + 465 = 807.
 *
 */

struct ListNode {
    int val;
    struct ListNode* next;
};

struct ListNode* addTwoNumbers(struct ListNode* l1, struct ListNode* l2) {
    struct ListNode* result  = NULL;
    struct ListNode* current = NULL;

    /* Carry from the previous operation */
    int carry = 0;

    while (l1 != NULL || l2 != NULL || carry != 0) {
        /* First, allocate the first/next node in the result linked list */
        if (result == NULL) {
            result  = malloc(sizeof(struct ListNode));
            current = result;
        } else {
            current->next = malloc(sizeof(struct ListNode));
            current       = current->next;
        }

        int n1 = 0;
        int n2 = 0;

        /* If a pointer is valid, save the value and move to the next one for
         * the next iteration. Otherwise, the value is a leading zero. */
        if (l1 != NULL) {
            n1 = l1->val;
            l1 = l1->next;
        }

        if (l2 != NULL) {
            n2 = l2->val;
            l2 = l2->next;
        }

        /* Add the two digits, and the carry from the last iteration */
        int sum = n1 + n2 + carry;

        /* Save right-most digit of result, and carry */
        current->val = sum % 10;
        carry        = sum / 10;
    }

    current->next = NULL;
    return result;
}

int main(void) {
    struct ListNode* test_case = malloc(11 * sizeof(struct ListNode));
    struct ListNode* result;

    /* 2 -> 4 -> 3 */
    test_case[0].val  = 2;
    test_case[0].next = &test_case[1];
    test_case[1].val  = 4;
    test_case[1].next = &test_case[2];
    test_case[2].val  = 3;
    test_case[2].next = NULL;

    /* 5 -> 6 -> 4 */
    test_case[3].val  = 5;
    test_case[3].next = &test_case[4];
    test_case[4].val  = 6;
    test_case[4].next = &test_case[5];
    test_case[5].val  = 4;
    test_case[5].next = NULL;

    printf("Result 1: ");
    result = addTwoNumbers(&test_case[0], &test_case[3]);
    while (result != NULL) {
        printf("%d", result->val);

        if (result->next != NULL)
            printf(" -> ");

        result = result->next;
    }
    putchar('\n');

    /* 9 -> 9 -> 9 -> 9 -> 9 -> 9 -> 9 */
    for (int i = 0; i <= 5; i++) {
        test_case[i].val  = 9;
        test_case[i].next = &test_case[i + 1];
    }
    test_case[6].val  = 9;
    test_case[6].next = NULL;

    /* 9 -> 9 -> 9 -> 9 */
    for (int i = 7; i <= 9; i++) {
        test_case[i].val  = 9;
        test_case[i].next = &test_case[i + 1];
    }
    test_case[10].val  = 9;
    test_case[10].next = NULL;

    printf("Result 2: ");
    result = addTwoNumbers(&test_case[0], &test_case[7]);
    while (result != NULL) {
        printf("%d", result->val);

        if (result->next != NULL)
            printf(" -> ");

        result = result->next;
    }
    putchar('\n');

    free(result);
    free(test_case);
    return 0;
}
