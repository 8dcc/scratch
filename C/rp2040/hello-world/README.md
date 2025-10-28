Simple UART
===========

Simple hello world example through the USB for the RP2040.

Dependencies
------------

Clone the Pico SDK, along with its submodules.

```bash
git clone --recurse-submodules https://github.com/raspberrypi/pico-sdk
```

Export the path as an environment variable. This will be used by the Pico SDK
(`pico_sdk_import.cmake`).

```bash
export PICO_SDK_PATH="$(pwd)/pico-sdk"
```

Next, download the `arm-none-eabi` version of the ARM Cross Toolchain from [the
official website](https://developer.arm.com/downloads/-/arm-gnu-toolchain-downloads),
selecting your host. For example:

```bash
curl -O https://developer.arm.com/-/media/Files/downloads/gnu/14.3.rel1/binrel/arm-gnu-toolchain-14.3.rel1-x86_64-arm-none-eabi.tar.xz
tar xf arm-gnu-toolchain-14.3.rel1-x86_64-arm-none-eabi.tar.xz
export PICO_TOOLCHAIN_PATH="$(pwd)/arm-gnu-toolchain-14.3.rel1-x86_64-arm-none-eabi"
```

Furthermore, the `PICO_PLATFORM` environment variable can be defined to
`rp2040`, although it is the default.

```bash
export PICO_PLATFORM='rp2040'
```

Note that most of these export can be avoided by launching `cmake` with the
`-DVARIABLE=VALUE` syntax, replacing `VARIABLE` and `ARGUMENT`.

Building
--------

Build the project with CMake.

```bash
mkdir build
cd build
cmake ..
```

Then, connect the MicroUSB port of the Raspberry Pi Pico to your computer while
holding its button, and copy the generated `.uf2` file to the mass storage
device.

The board should flash and restart. You can now attach your UART device, etc.
