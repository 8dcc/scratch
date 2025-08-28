#!/bin/bash

# NOTE: This script is used to redirect the output of the python script to a
# virtual sink that can be used as an input device for apps like discord.
#
# Credits: https://nebu.substack.com/p/linux-pulseaudio-how-to-stream-audio

# Name of the output device, usually your headphones. This is so the user also
# hears the audio. The name is obtained from the following command:
#   $ pactl list sinks
#     ...
#     Sink #1
#         State: RUNNING
#         Name: alsa_output.pci-0000_00_1f.3.analog-stereo        <-------------
#         Description: Built-in Audio Analog Stereo
#     ...
DEFAULT_SINK=alsa_output.pci-0000_00_1f.3.analog-stereo

# Create virtual sink for the python program
pactl load-module module-null-sink sink_name=V1 sink_properties=device.description=V1_PythonSink

# Create a loopback so the python output that went to V1 also goes to our
# headphones
pactl load-module module-loopback source=V1.monitor sink=$DEFAULT_SINK

echo "TODO: Play something from the python program, and from pavucontrol,"
echo "change the output of the python program (something like ffplay)."
