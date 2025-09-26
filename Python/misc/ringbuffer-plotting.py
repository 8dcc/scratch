#!/usr/bin/env python3

import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

class RingBuffer:
    def __init__(self, size):
        self.size = size  # Fixed number of elements at all times
        self.buf = []
        for _ in range(self.size):
            self.buf.append(None)

    def getSize(self):
        return self.size

    def getBuffer(self):
        return self.buf

    def append(self, element):
        self.buf.pop(0)  # Discard first element, shifting array to the left
        self.buf.append(element)

class RingBufferPlotter:
    def __init__(self, ringbuffer, interval=20):
        self.ringbuffer = ringbuffer
        self.interval = interval  # Update interval in ms

        self.fig, self.ax = plt.subplots()
        assert isinstance(self.ax, plt.Axes)  # Not an array

        # Set plot title.
        self.ax.set_title("RingBuffer contents over time")

        # For LaTeX-like style.
        plt.rcParams['font.family'] = 'serif'
        plt.rcParams['font.serif'] = [
            'Computer Modern',
            'DejaVu Serif',
            'Times New Roman',
            'serif',
        ]
        plt.rcParams['axes.linewidth'] = 0.8  # Thinner axis lines
        plt.rcParams['xtick.direction'] = 'in'  # Ticks inside
        plt.rcParams['ytick.direction'] = 'in'

    def on_update(self, frame):
        data = self.ringbuffer.getBuffer()

        # Clear axis data.
        self.ax.clear()

        # Set the bottom limit to 0, and the top limit to the highest non-nil
        # value.
        self.ax.set_ylim(0, max([d + 1 for d in data if d is not None] + [1]))

        # Plot the non-nil ringbuffer data using a line graph.
        self.ax.plot(range(len(data)), [d if d is not None else 0 for d in data])

    def show(self):
        # Bind the animation controller to a variable to avoid Python's garbage
        # collection.
        ani = FuncAnimation(self.fig, self.on_update, interval=self.interval)
        plt.show()

# ------------------------------------------------------------------------------

import random
import time
import threading

# Background thread for modifying the ringbuffer
def update_loop(ringbuffer):
    while True:
        for i in list(range(100)) + list(range(99, -1, -1)):
            ringbuffer.append(i + random.randint(0, 20))
            time.sleep(0.01)

def main():
    ringbuffer = RingBuffer(1000)
    plotter = RingBufferPlotter(ringbuffer)

    # Update ringbuffer in one thread
    threading.Thread(target=update_loop, args=(ringbuffer,), daemon=True).start()

    # Show plotter in main thread
    plotter.show()

if __name__ == "__main__":
    main()
