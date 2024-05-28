/* Kata: https://www.codewars.com/kata/55d24f55d7dd296eb9000030 */

int summation_clean(int num) {
    int r = 0;
    for (int n = 1; n <= num; n++) {
        r += n;
    }
    return r;
}

int summation_ugly(int num) {
    int r = 0;
    for (; num > 0; num--) r += num;
    return r;
}
