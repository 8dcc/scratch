/* Kata: https://www.codewars.com/kata/541c8630095125aba6000c00 */

int digital_root(int n) {
    int buf = n;
    int final = 0;
    while (buf > 9) {           // Keep adding until the result is one digit
        final = 0;              // Reset new sum
        while (buf) {           // Keep adding until we added all the digits
            final += buf % 10;  // Add last digit to the result
            buf = buf / 10;     // Remove last digit
        }
        buf = final;
    }
    return final;
}
