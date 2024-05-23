/* Compile with -g and debug with gdb */

#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>    // htonl(), htons()

int main() {
    uint32_t num  = 41136;         // 0xA0B0
    uint32_t num2 = htonl(num);    // 0xB0A00000 (4 bytes)
    uint16_t num3 = htons(num);    // 0xB0A0 (2 bytes)

    printf("%d | %d | %d\n", num, num2, num3);

    return 0;
}

