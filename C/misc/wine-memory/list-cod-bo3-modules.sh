#!/bin/bash

EXECUTABLE="BlackOps3.exe"

pids=$(pgrep -f $EXECUTABLE)

if [ "$1" = "--simple" ]; then
    for pid in $pids; do
        modules=$(cat "/proc/$pid/maps" | grep "$EXECUTABLE")
        if [ ! -z "$modules" ]; then
            echo "$pid"
            exit 0
        fi
    done
else
    for pid in $pids; do
        modules=$(cat "/proc/$pid/maps" | grep "$EXECUTABLE")

        if [ $(tput colors) ]; then
            color_reset='\x1b[0m'

            if [ -z "$modules" ]; then
                color='\x1b[31m'
            else
                color='\x1b[1;32m'
            fi
        else
            color_reset=''
            color=''
        fi

        echo -e "----- $color$pid$color_reset -----"

        # Yes, we call this command twice
        cat "/proc/$pid/maps" | grep "$EXECUTABLE"
    done
fi
