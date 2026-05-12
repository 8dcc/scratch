#!/usr/bin/env python3
#
# Copyright 2026 8dcc. All Rights Reserved.
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
#
# ------------------------------------------------------------------------------
#
# Bluetooth RFCOMM man-in-the-middle for ELM327 traffic.
#
# Listens for an incoming RFCOMM connection (e.g. from Torque) and forwards
# all traffic to a real ELM327 adapter, logging both directions to stdout.
#
# Usage:
#   python3 elm327_proxy.py <adapter-mac> [server-channel] [adapter-channel]
#
# Setup:
#   1. Pair your PC with the real OBD2 adapter beforehand.
#   2. Run this script. It will connect to the adapter then wait for a client.
#   3. In the target client (e.g. Torque), change the adapter MAC to the PC's
#      Bluetooth MAC address.

import socket
import sys
import threading

ADAPTER_MAC     = sys.argv[1] if len(sys.argv) > 1 else None
SERVER_CHANNEL  = int(sys.argv[2]) if len(sys.argv) > 2 else 1
ADAPTER_CHANNEL = int(sys.argv[3]) if len(sys.argv) > 3 else 1

if ADAPTER_MAC is None:
    print("Usage: elm327_proxy.py <adapter-mac> [server-channel] [adapter-channel]")
    sys.exit(1)

# ------------------------------------------------------------------------------

def format_data(data: bytes) -> str:
    try:
        text = data.decode("ascii")
        # Show newlines explicitly so single-line log entries are readable
        text = text.replace("\r", "\\r").replace("\n", "\\n")
        return repr(text)[1:-1]
    except UnicodeDecodeError:
        return data.hex(" ")

def pipe(src: socket.socket, dst: socket.socket, label: str, stop: threading.Event):
    """Forward bytes from src to dst, logging each chunk."""
    try:
        while not stop.is_set():
            chunk = src.recv(256)
            if not chunk:
                break
            print(f"  {label}  {format_data(chunk)}")
            dst.sendall(chunk)
    except OSError:
        pass
    finally:
        stop.set()

# ------------------------------------------------------------------------------

def main():
    # Connect to the real adapter first so we fail fast if it is unreachable.
    print(f"Connecting to adapter {ADAPTER_MAC} on channel {ADAPTER_CHANNEL}...")
    adapter_sock = socket.socket(socket.AF_BLUETOOTH,
                                 socket.SOCK_STREAM,
                                 socket.BTPROTO_RFCOMM)
    adapter_sock.connect((ADAPTER_MAC, ADAPTER_CHANNEL))
    print("Connected to adapter.")

    # Now listen for the client (e.g. Torque).
    server_sock = socket.socket(socket.AF_BLUETOOTH,
                                socket.SOCK_STREAM,
                                socket.BTPROTO_RFCOMM)
    server_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_sock.bind(("00:00:00:00:00:00", SERVER_CHANNEL))
    server_sock.listen(1)
    print(f"Waiting for client on RFCOMM channel {SERVER_CHANNEL}...")

    try:
        client_sock, client_addr = server_sock.accept()
    except KeyboardInterrupt:
        print("\n[aborted]")
        adapter_sock.close()
        server_sock.close()
        return

    print(f"Client connected: {client_addr}\n")
    server_sock.close()

    stop = threading.Event()

    # CLIENT → ADAPTER
    t1 = threading.Thread(
        target=pipe,
        args=(client_sock, adapter_sock, "client → adapter:", stop),
        daemon=True,
    )
    # ADAPTER → CLIENT
    t2 = threading.Thread(
        target=pipe,
        args=(adapter_sock, client_sock, "adapter → client:", stop),
        daemon=True,
    )

    t1.start()
    t2.start()

    try:
        stop.wait()
    except KeyboardInterrupt:
        stop.set()

    print("\n[disconnected]")
    client_sock.close()
    adapter_sock.close()


if __name__ == "__main__":
    main()
