#!/usr/bin/env bash
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
set -e

# Name of the virtual sink to be created.
VIRTUAL_SINK='MergedAudioSink'

# The name of the sources is obtained from the following command:
#    $ pactl list sources short
#    0 alsa_output.pci-0000_01_00.1.hdmi-stereo.monitor [...]
#    1 alsa_output.pci-0000_00_1f.3.analog-stereo.monitor [...]
#    2 alsa_input.pci-0000_00_1f.3.analog-stereo [...]
#    ...
SOURCE_A='alsa_output.pci-0000_00_1f.3.analog-stereo.monitor'
SOURCE_B='alsa_input.pci-0000_00_1f.3.analog-stereo'

# Create virtual sink which will receibe both sources (e.g. desktop and
# microphone audios).
pactl load-module module-null-sink sink_name="$VIRTUAL_SINK" sink_properties=device.description="$VIRTUAL_SINK"

# Create a loopback so that the source A (e.g. desktop audio) goes to the
# virtual sink we just created.
pactl load-module module-loopback source="$SOURCE_A" sink="$VIRTUAL_SINK"

# Create a loopback so that the source B (e.g. microphone) goes to the virtual
# sink we just created.
pactl load-module module-loopback source="$SOURCE_B" sink="$VIRTUAL_SINK"

echo "Merged '$SOURCE_A' and '$SOURCE_B' into '$VIRTUAL_SINK'."
