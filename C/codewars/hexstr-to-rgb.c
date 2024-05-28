/* Kata: https://www.codewars.com/kata/5282b48bb70058e4c4000fa7 */

typedef struct {
    int r, g, b;
} rgb;

int htoi(const char* hex);

rgb hex_str_to_rgb(const char *hex_str) {
    rgb ret = {0, 0, 0};
    
    ret.r = htoi(&hex_str[1]);
    ret.g = htoi(&hex_str[3]);
    ret.b = htoi(&hex_str[5]);

    return ret;
}

// For 2 char hex strs
int htoi(const char* hex) {
    int pos = 0, result = 0;

    if (hex[pos] >= '0' && hex[pos] <= '9')
        result += (hex[pos] - '0') * 16;        // Add numeric value from char * 16
    else if (hex[pos] >= 'a' && hex[pos] <= 'f')
        result += (hex[pos] - 'a' + 10) * 16;
    else if (hex[pos] >= 'A' && hex[pos] <= 'F')
        result += (hex[pos] - 'A' + 10) * 16;
    else
        return -1;

    pos++;
    
    if (hex[pos] >= '0' && hex[pos] <= '9')
        result += hex[pos] - '0';
    else if (hex[pos] >= 'a' && hex[pos] <= 'f')
        result += (hex[pos] - 'a' + 10);
    else if (hex[pos] >= 'A' && hex[pos] <= 'F')
        result += (hex[pos] - 'A' + 10);
    else
        return -1;

    return result;
}
