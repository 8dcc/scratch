
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
 * Initial solution
 * ----------------
 *
 * My initial solution did not use a dummy node. See the previous commit or:
 * https://leetcode.com/problems/add-two-numbers/solutions/5666309/simple-o-n-solution-in-c/
 *
 */

struct ListNode {
    int val;
    struct ListNode* next;
};

struct ListNode* addTwoNumbers(struct ListNode* l1, struct ListNode* l2) {
    /* Dummy node declared on the stack for a avoiding conditional inside the
     * loop. */
    struct ListNode dummy;

    /* Actual value to be returned */
    dummy.next = NULL;

    struct ListNode* current = &dummy;

    /* Carry from the previous operation */
    int carry = 0;

    while (l1 != NULL || l2 != NULL || carry != 0) {
        /* Initialize the sum as the carry from the last iteration */
        int sum = carry;

        /* If a pointer is valid, save the value and move to the next one for
         * the next iteration. Otherwise, the value is a leading zero. */
        if (l1 != NULL) {
            sum += l1->val;
            l1 = l1->next;
        }

        if (l2 != NULL) {
            sum += l2->val;
            l2 = l2->next;
        }

        /* Allocate the next node in the result linked list */
        current->next = malloc(sizeof(struct ListNode));
        current       = current->next;
        current->next = NULL;

        /* Save right-most digit of result, and carry */
        current->val = sum % 10;
        carry        = sum / 10;
    }

    return dummy.next;
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
