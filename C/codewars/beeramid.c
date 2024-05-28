/* Kata: https://www.codewars.com/kata/51e04f6b544cf3f6550000c1 */

# include <stdio.h>

// Returns number of complete beeramid levels
int beeramid(double bonus, double price) {
    int total_cans = bonus / price;

    int result = -1;
    int side_cans = 1;
    while (total_cans >= 0) {
        total_cans -= side_cans * side_cans;
        side_cans++;
        result++;
        /* printf("Cans: %d | SCans: %d | Res: %d\n", total_cans, side_cans, result); */
    }

    return result;
}

// Testing
int main() {
    int test_bonus = 10;
    int test_price = 2;

    beeramid(test_bonus, test_price);

    return 0;
}
