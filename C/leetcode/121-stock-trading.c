
#include <stdio.h>

/*
 * https://leetcode.com/problems/best-time-to-buy-and-sell-stock/
 *
 * 121. Best Time to Buy and Sell Stock
 * ====================================
 *
 * You are given an array `prices' where `prices[i]' is the price of a given
 * stock on the i-th day.
 *
 * You want to maximize your profit by choosing a single day to buy one stock
 * and choosing a different day in the future to sell that stock.
 *
 * Return the maximum profit you can achieve from this transaction. If you
 * cannot achieve any profit, return 0.
 *
 * Example
 * -------
 *
 * - Input: prices = [7,1,5,3,6,4]
 * - Output: 5
 * - Explanation: Buy on day 2 (price = 1) and sell on day 5 (price = 6),
 *   profit = 6-1 = 5. Note that buying on day 2 and selling on day 1 is not
 *   allowed because you must buy before you sell.
 *
 * Notes
 * -----
 *
 * This is the graph for the first test case: [7,1,5,3,6,4]
 *
 *   (Price)
 *      |
 *      | *
 *    6 |             *
 *      |       *
 *    4 |                *
 *      |          *
 *    2 |
 *      |    *
 *    0 |
 *      +------------------ (Day)
 *        0  1  2  3  4  5
 *
 * - You obviously need to buy prior to selling.
 * - In order to make profit, the stock when buying has to be lower than when
 *   selling.
 * - To get the maximum profit, you need to buy when the stock is the lowest,
 *   and sell when the stock is the highest.
 * - We are not asked to return the best days for buying or selling, just the
 *   difference between them. Therefore, we don't need to worry about
 *   overwriting these "buy" and "sell" day values, we just need to store the
 *   highest difference between them.
 */

int maxProfit(int* prices, int pricesSize) {
    int result = 0;

    /* We could initialize `sell' to `&prices[1]' if we knew that `pricesSize'
     * is greater or equal than 2, but that's not the case. */
    int* buy  = &prices[0];
    int* sell = &prices[0];

    /* Iterate each price */
    while (pricesSize-- > 0) {
        if (*buy < *sell) {
            /* If the current buy price is lower than the current sell price,
             * calculate the profit. If this is the biggest profit we
             * encountered, save it. */
            int profit = *sell - *buy;
            if (result < profit)
                result = profit;
        } else {
            /* Otherwise, we found a lower buy price than before. Start using
             * that as the buy price from now on. */
            buy = sell;
        }

        /* Each iteration we want to increment the selling price either way. */
        sell++;
    }

    return result;
}

int main(void) {
    int case1[6] = { 7, 1, 5, 3, 6, 4 };
    printf("Result 1: %d\n", maxProfit(case1, 6));

    int case2[5] = { 7, 6, 4, 3, 1 };
    printf("Result 2: %d\n", maxProfit(case2, 5));

    return 0;
}
