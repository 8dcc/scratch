#!/usr/bin/env python3
#
# Copyright 2025 8dcc. All Rights Reserved.
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https://www.gnu.org/licenses/>.

import time
import sys
import threading
from datetime import datetime
from numbers import Number

import obd
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

RINGBUFFER_SIZE = 100  # Elements
OBD_QUERY_DELAY = 0.1  # Seconds

# Some ECUs have a bug that makes them report ~250 RPMs when the engine is
# off. This affects some Renault, Dacia and Nissan ECUs.
FIX_RPM_BUG = True

class RingBuffer:
    def __init__(self, name, size):
        self.name = name  # For identification when plotting
        self.size = size  # Fixed number of elements at all times
        self.buf = []
        for _ in range(self.size):
            self.buf.append(None)

    def getName(self):
        return self.name

    def getSize(self):
        return self.size

    def getBuffer(self):
        return self.buf

    def append(self, element):
        self.buf.pop(0)  # Discard first element, shifting array to the left
        self.buf.append(element)

class RingBufferPlotter:
    def __init__(self, ringbuffers, interval=100):
        self.ringbuffers = ringbuffers
        self.interval = interval  # Update interval in ms

        self.axis_colors = [
            'blue', 'red', 'green', 'purple', 'orange', 'cyan', 'magenta',
            'black', 'darkblue', 'darkred', 'darkgreen', 'darkviolet', 'gold',
            'darkcyan', 'hotpink', 'brown', 'lime', 'teal', 'olive', 'maroon',
            'navy', 'coral', 'crimson', 'turquoise', 'indigo', 'salmon',
            'violet', 'limegreen', 'darkorange'
        ]

        self.fig, self.ax = plt.subplots()
        assert isinstance(self.ax, plt.Axes)  # Not an array

        # FIXME: Document
        self.fig.subplots_adjust(left=0.1, right=0.70)

        # Initialize the plot axes once.
        self.axes = []
        for i in range(len(self.ringbuffers)):
            # Use the base axis for the first ringbuffer, create new axes for
            # the rest.
            cur_ax = self.ax if i == 0 else self.ax.twinx()

            # Store the axis
            self.axes.append(cur_ax)

        # Set plot title.
        self.ax.set_title("RingBuffer contents over time")

        # For LaTeX-like style.
        plt.rcParams["font.family"] = "serif"
        plt.rcParams["font.serif"] = [
            "Computer Modern",
            "DejaVu Serif",
            "Times New Roman",
            "serif",
        ]
        plt.rcParams["axes.linewidth"] = 0.8  # Thinner axis lines
        plt.rcParams["xtick.direction"] = "in"  # Ticks inside
        plt.rcParams["ytick.direction"] = "in"

    def on_update(self, frame):
        # Iterate over all ringbuffers and their corresponding axes
        axes = []
        for i, (cur_ringbuffer, cur_ax) in enumerate(zip(self.ringbuffers, self.axes), 0):
            cur_buffer = cur_ringbuffer.getBuffer()

            # Current color the ringbuffer line and the axis label.
            cur_color = self.axis_colors[i % len(self.axis_colors)]

            # Clear current axis data.
            cur_ax.clear()

            # Set the Y-axis label to the current ringbuffer name, and the label
            # color. If it's an aditional right-side axis, move it to avoid
            # overlap.
            cur_ax.set_ylabel(cur_ringbuffer.getName())
            cur_ax.yaxis.label.set_color(cur_color)
            if i > 0:
                cur_ax.yaxis.set_label_position("right")
                cur_ax.yaxis.set_ticks_position("right")
                cur_ax.spines.right.set_position(("outward", (i - 1) * 50))

            # Set the bottom limit to 0, and the top limit to the highest
            # non-nil value.
            cur_ax.set_ylim(0, max([d + 1 for d in cur_buffer if d is not None] + [1]))

            # Plot the non-nil ringbuffer data using a line graph.
            cur_ax.plot(
                range(len(cur_buffer)),
                [d if d is not None else 0 for d in cur_buffer],
                color=cur_color,
            )

        return self.axes

    def show(self):
        # Bind the animation controller to a variable to avoid Python's garbage
        # collection.
        ani = FuncAnimation(self.fig, self.on_update, interval=self.interval)
        plt.show()

# ------------------------------------------------------------------------------

def query_num_cmd(obd_connection, command):
    time.sleep(OBD_QUERY_DELAY)

    # Query the current command.
    query_response = obd_connection.query(command)
    if not query_response or query_response.is_null():
        wrn("Got NULL query response.")
        return None

    # If the current query response has a "magnitude" attribute, it
    # contains the unit, so we need to further extract the value.
    query_value = query_response.value
    if hasattr(query_value, "magnitude"):
        query_value = query_value.magnitude

    # We can only plot numbers.
    if not isinstance(query_value, Number):
        wrn(f"Got non-numeric query response: {query_value}")
        return None

    return query_value

class ComplexObdCmd:
    def __init__(self, name, commands, result_transformator=None):
        self.name = name
        self.commands = commands

        # Function to be called with an array of query results for each command.
        if callable(result_transformator):
            self.result_transformator = result_transformator
        else:
            self.result_transformator = lambda result_arr: result_arr[0]

    def getName(self):
        return self.name

    def getCommands(self):
        return self.commands

    def getFinalResult(self, obd_connection):
        # Get an array with the results of querying each command.
        query_results = list(map(
            lambda cmd: query_num_cmd(obd_connection, cmd),
            self.commands
        ))

        # Calculate the result of applying the transformator function to the
        # array of queried commands.
        return self.result_transformator(query_results)

# ------------------------------------------------------------------------------

def err(msg):
    sys.stderr.write("[Error] " + msg + "\n")

def wrn(msg):
    sys.stderr.write("[Warning] " + msg + "\n")

# Background thread for modifying the ringbuffer
def update_loop(obd_connection, ringbuffers, command_descriptors):
    assert len(ringbuffers) == len(command_descriptors)
    while True:
        for i in range(len(ringbuffers)):
            ringbuffers[i].append(command_descriptors[i].getFinalResult(obd_connection))

def main():
    if len(sys.argv) != 2:
        err(f"Usage: {sys.argv[0]} SERIAL-DEVICE")
        sys.exit(1)

    device_path = sys.argv[1]
    obd_connection = obd.OBD(device_path)
    if not obd_connection or not obd_connection.is_connected():
        err(f"Could not connect to '{device_path}'. Aborting.")
        sys.exit(1)

    print(f"Connected to  '{device_path}'.")
    print(f"  * Protocol: {obd_connection.protocol_name()}")

    # Try to query ELM version.
    adapter_info = obd_connection.query(obd.commands.ELM_VERSION)
    if adapter_info and not adapter_info.is_null():
       print(f"  * Adapter Version: {adapter_info.value}")
    print()

    command_descriptors = [
        # For RPMs, if the value falls in the "RPM bug" range, reset it to zero.
        ComplexObdCmd(
            "RPM",
            [obd.commands.RPM],
            lambda result_arr: 0 if FIX_RPM_BUG and 200 <= result_arr[0] <= 300 else result_arr[0],
        ),

        ComplexObdCmd("Speed", [obd.commands.SPEED]),
        ComplexObdCmd("Throttle %", [obd.commands.THROTTLE_POS]),
        ComplexObdCmd("Engine Load", [obd.commands.ENGINE_LOAD]),

        # Boost is calculated by subtracting the atmosferic pressure from intake
        # manifold pressure.
        #
        # NOTE: You should ideally calculate it with MONITOR_BOOST_PRESSURE_B1,
        # or calculate (INTAKE_PRESSURE - BAROMETRIC_PRESSURE). However, not all
        # cars support this, so a generic barometric pressure is used (101.3 kPa).
        ComplexObdCmd(
            "Boost",
            [obd.commands.INTAKE_PRESSURE],
            lambda result_arr: result_arr[0] - 101.3,
        ),
    ]

    # Verify they are all supported.
    for command_descriptor in command_descriptors:
        for command in command_descriptor.getCommands():
            if not obd.commands.has_command(command) or not obd_connection.supports(command):
                wrn(f"The current adapter doesn't support the '{command_descriptor.getName()}' command ({command}). Removing from list.")
                command_descriptors.remove(command_descriptor)
                break

    # Initialize the ringbuffer array.
    ringbuffers = []
    for command_descriptor in command_descriptors:
        ringbuffers.append(RingBuffer(
            command_descriptor.getName(),
            RINGBUFFER_SIZE,
        ))

    # Update ringbuffers with OBD data in a separate thread.
    threading.Thread(
        target=update_loop,
        args=(obd_connection, ringbuffers, command_descriptors),
        daemon=True
    ).start()

    # Show plotter in main thread.
    plotter = RingBufferPlotter(ringbuffers)
    plotter.show()

if __name__ == "__main__":
    main()
