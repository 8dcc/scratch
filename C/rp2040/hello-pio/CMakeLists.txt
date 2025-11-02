
# Required by the Pico SDK.
cmake_minimum_required(VERSION 3.13)

# Must be included before the project.
include(../pico_sdk_import.cmake)

# The Pico SDK requires both C and C++. We don't need to specify 'ASM'.
project(rp2040_hello_pio C CXX ASM)

# The Pico SDK requires at least C11.
set(CMAKE_C_STANDARD 11)

# Initialize the Pico SDK.
pico_sdk_init()

add_executable(rp2040_hello_pio
    main.c
)

# Generate the 'hello.pio.h' header from the 'hello.pio' assembly source, which
# will be included from 'main.h'.
pico_generate_pio_header(rp2040_hello_pio ${CMAKE_CURRENT_LIST_DIR}/hello.pio)

# Link with the Pico standard library, and with PIO support.
target_link_libraries(rp2040_hello_pio
    pico_stdlib
    hardware_pio
)

# Disable USB and UART outputs, to avoid distractions.
pico_enable_stdio_usb(rp2040_hello_pio 0)
pico_enable_stdio_uart(rp2040_hello_pio 0)

# Generate, among others, an UF2 file, needed for flashing to the Pico.
pico_add_extra_outputs(rp2040_hello_pio)
