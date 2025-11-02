
#include <ctype.h> /* toupper */

#include "pico/stdlib.h"
#include "hardware/uart.h"

#if !defined(PICO_DEFAULT_LED_PIN) || PICO_DEFAULT_LED_PIN >= NUM_BANK0_GPIOS
#error Expected a valid LED pin.
#endif

int main(void) {
    /*
     * Initialize UART 0 at 115200 baud.
     */
    uart_init(uart0, 115200);

    /*
     * Bind ports 0 and 1 (next to the Pico LED) to RX and TX, respectively.
     */
    gpio_set_function(0, GPIO_FUNC_UART);
    gpio_set_function(1, GPIO_FUNC_UART);

    /*
     * Initialize the LED GPIO, for showing activity.
     */
    gpio_init(PICO_DEFAULT_LED_PIN);
    gpio_set_dir(PICO_DEFAULT_LED_PIN, GPIO_OUT);

    bool led_toggle = false;
    for (;;) {
        /*
         * If data is available, read one character and print it back as
         * upercase.
         */
        if (uart_is_readable(uart0)) {
            const char c = uart_getc(uart0);
            uart_putc(uart0, toupper(c));

            gpio_put(PICO_DEFAULT_LED_PIN, led_toggle);
            led_toggle = !led_toggle;
        }
    }

    return 0;
}
