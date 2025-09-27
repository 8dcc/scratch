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

#!/usr/bin/env bash
set -e

if [ $# -ne 1 ]; then
    echo "Usage: $(basename "$0") DEVICE-MAC" 1>&2
    exit 1
fi

MAC="$1"

# ------------------------------------------------------------------------------

sudo systemctl start bluetooth
sleep 1

sudo systemctl status --lines 0 bluetooth # Validate

echo "---------------------------------------------"

bluetoothctl agent on
bluetoothctl power on
bluetoothctl scan on

set +e
bluetoothctl pair "$MAC"  # Might fail (already paired)
set -e

bluetoothctl trust "$MAC"

set +e
bluetoothctl connect "$MAC"  # Might fail (while also connecting)
set -e

echo "---------------------------------------------"

sudo rfcomm release 0  # Might fail, but returns 0
sudo rfcomm bind 0 "$MAC" 1
rfcomm  # Validate
