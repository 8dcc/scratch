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

import obd
import time
import sys
from datetime import datetime

def err(msg):
    sys.stderr.write(msg + "\n")

def main():
    if len(sys.argv) != 2:
        err(f"Usage: {sys.argv[0]} SERIAL-DEVICE")
        exit(1)

    device_path = sys.argv[1]
    connection = obd.OBD(device_path)
    if not connection or not connection.is_connected():
        err(f"Could not connect to '{device_path}'. Aborting.")
        sys.exit(1)

    print(f"Connected to '{device_path}'.")
    print(f"  * Protocol: {connection.protocol_name()}")

    # Try to query ELM version.
    adapter_info = connection.query(obd.commands.ELM_VERSION)
    if adapter_info and not adapter_info.is_null():
       print(f"  * Adapter Version: {adapter_info.value}")
    print()

    try:
        dtcs = connection.query(obd.commands.GET_DTC)
        if dtcs.value:
            print(f"Dumping {len(dtcs.value)} trouble codes:")
            for code, desc in dtcs.value:
                print(f"  * {code}: {desc}")
        else:
            print("No trouble codes found.")
    finally:
        connection.close()

if __name__ == "__main__":
    main()
