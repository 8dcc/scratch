
#include "pico/stdlib.h"
#include "hardware/pio.h"
#include "hardware/clocks.h"

/*
 * Include the header that the 'CMakeLists.txt' generates from our 'hello.pio'
 * assembly program.
 */
#include "hello.pio.h"

int main(void) {
    /*
     * Find a free PIO state machine for our program, and load it for us.
     */
    PIO pio     = pio0;
    uint offset = pio_add_program(pio, &hello_program);

    /*
     * Configure the PIO using the function that we defined in 'hello.pio', and
     * load it into the state machine zero.
     */
    uint sm = 0;
    hello_program_init(pio, sm, offset, PICO_DEFAULT_LED_PIN);

    for (;;) {
        /*
         * Write the number 1 to the state machine's TX FIFO, which will get
         * moved to the Output Shift Register by the 'pull' instruction of the
         * 'hello.pio' file.
         *
         * Keep turning the led ON and OFF using this method.
         */
        pio_sm_put_blocking(pio, sm, 1);
        sleep_ms(100);
        pio_sm_put_blocking(pio, sm, 0);
        sleep_ms(900);
    }

    /*
     * Free resources, and unload our program.
     */
    pio_remove_program_and_unclaim_sm(&hello_program, pio, sm, offset);

    return 0;
}
