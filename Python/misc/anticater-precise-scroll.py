#!/usr/bin/env python3
#
# Grabs the Anticater knob and re-emits fractional hi-res scroll events.

import evdev
from evdev import InputDevice, UInput, ecodes
import time

# Obtain from 'lsusb'
VENDOR  = 0x514c
PRODUCT = 0x8850

# Scroll step: 120 = full tick, 30 = quarter tick
HI_RES_STEP = 30

def find_device():
    for path in evdev.list_devices():
        dev = InputDevice(path)
        if dev.info.vendor == VENDOR and dev.info.product == PRODUCT:
            # We want the keyboard interface (emits KEY_VOLUMEUP/DOWN)
            if ecodes.EV_KEY in dev.capabilities():
                caps = dev.capabilities()[ecodes.EV_KEY]
                if ecodes.KEY_VOLUMEUP in caps:
                    return dev
    raise RuntimeError("Anticater device not found")

def main():
    dev = find_device()
    dev.grab()  # take exclusive control

    ui = UInput({
        ecodes.EV_REL: [
            ecodes.REL_WHEEL_HI_RES,
            ecodes.REL_WHEEL,
        ],
        ecodes.EV_KEY: [
            ecodes.KEY_PAGEDOWN,
            ecodes.KEY_PAGEUP,
            ecodes.KEY_PLAYPAUSE,
        ],
    }, name="anticater-scroll", version=0x1)

    print(f"Grabbed {dev.name}, emitting hi-res scroll (step={HI_RES_STEP})")

    try:
        for event in dev.read_loop():
            if event.type != ecodes.EV_KEY:
                continue
            if event.value != 1:  # key down only
                continue

            if event.code == ecodes.KEY_VOLUMEUP:
                ui.write(ecodes.EV_REL, ecodes.REL_WHEEL_HI_RES, -HI_RES_STEP)
                ui.syn()
            elif event.code == ecodes.KEY_VOLUMEDOWN:
                ui.write(ecodes.EV_REL, ecodes.REL_WHEEL_HI_RES, HI_RES_STEP)
                ui.syn()
            elif event.code == ecodes.KEY_BRIGHTNESSDOWN:
                ui.write(ecodes.EV_KEY, ecodes.KEY_PAGEUP, 1)
                ui.write(ecodes.EV_KEY, ecodes.KEY_PAGEUP, 0)
                ui.syn()
            elif event.code == ecodes.KEY_BRIGHTNESSUP:
                ui.write(ecodes.EV_KEY, ecodes.KEY_PAGEDOWN, 1)
                ui.write(ecodes.EV_KEY, ecodes.KEY_PAGEDOWN, 0)
                ui.syn()
            elif event.code == ecodes.KEY_MUTE:
                ui.write(ecodes.EV_KEY, ecodes.KEY_PLAYPAUSE, 1)
                ui.write(ecodes.EV_KEY, ecodes.KEY_PLAYPAUSE, 0)
                ui.syn()
    finally:
        ui.close()
        dev.ungrab()

if __name__ == "__main__":
    main()
