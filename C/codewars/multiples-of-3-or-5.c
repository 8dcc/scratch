/* Kata: https://www.codewars.com/kata/514b92a657cdc65150000006 */

int solution(int num) {
    int r = 0;
    if (num < 0) return 0;
    for (int n = 1; n < num; n++) {
        if (!(n % 3) || !(n % 5)) r += n;   // !(...) Means it is 0 (divisible)
    }
    return r;
}
