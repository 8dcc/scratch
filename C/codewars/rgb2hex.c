/* 
 * Kata: https://www.codewars.com/kata/513e08acc600c94f01000001
 * For more details see:
 * https://www.rapidtables.com/convert/number/decimal-to-hex.html
 */

void int2hex(int i, char* h);

int rgb(int r, int g, int b, char* output) {
    // We give the addresses of each part instead of making more than 1 str
    int2hex(r, output);
    int2hex(g, &output[2]);
    int2hex(b, &output[4]);
    return 0;
}

/* int2hex: Return a 2 digit uppercase string with the hex value of a number */
void int2hex(int i, char* h) {
    if (i < 0) i = 0;           // Make sure that (0 < i < 255)
    if (i > 255) i = 255;

    // First we do the second character
    int num2 = (i % 16 == 0) ? 0 : i % 16;
    if (num2 > 9) h[1] = num2 - 10 + 'A';   // If >= 10 use uppercase letters
    else h[1] = num2 + '0';
    // Then the first one
    int num1 = ((i / 16) % 16 == 0) ? 0 : (i / 16) % 16;
    if (num1 > 9) h[0] = num1 - 10 + 'A';
    else h[0] = num1 + '0';
}
