// 2-3 (Not 2-2)

#include <stdio.h>
#include <ctype.h>      // For tolower

int htoi(const char* hex);

int main() {
    // Tests
    const char* hex1 = "13";
    printf("H: %4s | I: %d\n", hex1, htoi(hex1));
    const char* hex2 = "0x13";                
    printf("H: %4s | I: %d\n", hex2, htoi(hex2));
    const char* hex3 = "1a";                  
    printf("H: %4s | I: %d\n", hex3, htoi(hex3));
    const char* hex4 = "b3";                  
    printf("H: %4s | I: %d\n", hex4, htoi(hex4));
    const char* hex5 = "0xb3";                
    printf("H: %4s | I: %d\n", hex5, htoi(hex5));
    const char* hex6 = "7b";                  
    printf("H: %4s | I: %d\n", hex6, htoi(hex6));
    const char* hex7 = "9a";                  
    printf("H: %4s | I: %d\n", hex7, htoi(hex7));
    const char* hex8 = "0x5z";                
    printf("H: %4s | I: %d\n", hex8, htoi(hex8));

    return 0;
}

int htoi(const char* hex) {
    int start_pos = 0;
    if (hex[0] == '0' && hex[1] == tolower('x') && hex[2] != '\0')  // hex starts with 0x or 0X and continues
        start_pos = 2;
    
    int pos = start_pos, result = 0;
    if (hex[pos] >= '0' && hex[pos] <= '9')
        result += (hex[pos] - '0') * 16;        // Add numeric value from char * 16
    else if (hex[pos] >= 'a' && hex[pos] <= 'f')
        result += (hex[pos] - 'a' + 10) * 16;
    else if (hex[pos] >= 'A' && hex[pos] <= 'F')
        result += (hex[pos] - 'A' + 10) * 16;
    else return -1;
    pos++;
    if (hex[pos] >= '0' && hex[pos] <= '9')
        result += hex[pos] - '0';
    else if (hex[pos] >= 'a' && hex[pos] <= 'f')
        result += (hex[pos] - 'a' + 10);
    else if (hex[pos] >= 'A' && hex[pos] <= 'F')
        result += (hex[pos] - 'A' + 10);
    else return -1;

    return result;
}


