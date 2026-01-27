
# Required by the Pico SDK.
cmake_minimum_required(VERSION 3.13)

# Must be included before the project.
include(../pico_sdk_import.cmake)

# The Pico SDK requires both C and C++. We don't need to specify 'ASM'.
project(rp2040_gps6mv2 C CXX)

# The Pico SDK requires at least C11.
set(CMAKE_C_STANDARD 11)

# Initialize the Pico SDK.
pico_sdk_init()

add_executable(rp2040_gps6mv2
    main.c
)

# Link with the Pico standard library and hardware UART.
target_link_libraries(rp2040_gps6mv2
    pico_stdlib
    hardware_uart
)

# Enable USB output, but disable standard UART GPIOs.
pico_enable_stdio_usb(rp2040_gps6mv2 1)
pico_enable_stdio_uart(rp2040_gps6mv2 0)

# Generate, among others, an UF2 file, needed for flashing to the Pico.
pico_add_extra_outputs(rp2040_gps6mv2)
