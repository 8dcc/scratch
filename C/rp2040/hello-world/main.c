
#include <stdio.h>

#include "pico/stdlib.h"

#if !defined(PICO_DEFAULT_LED_PIN) || PICO_DEFAULT_LED_PIN >= NUM_BANK0_GPIOS
#error Expected a valid LED pin.
#endif

int main(void) {
    /*
     * Initialize standard I/O, which we defined to be USB in 'CMakeLists.txt'.
     */
    stdio_init_all();

    /*
     * Wait a bit for USB connection, and print an arbitrary message through the
     * virtual serial port of the USB (using CDC).
     */
    sleep_ms(1000);
    printf("Hello, World!\n");

    /*
     * Initialize the LED GPIO, and show a heart-beat.
     */
    gpio_init(PICO_DEFAULT_LED_PIN);
    gpio_set_dir(PICO_DEFAULT_LED_PIN, GPIO_OUT);
    for (;;) {
        gpio_put(PICO_DEFAULT_LED_PIN, 1);
        sleep_ms(100);
        gpio_put(PICO_DEFAULT_LED_PIN, 0);
        sleep_ms(900);
    }

    return 0;
}
