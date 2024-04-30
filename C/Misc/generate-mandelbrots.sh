#!/bin/bash

w=1920
h=1080

base=1
end=255
step=2

rm -rf mandelbrots
mkdir mandelbrots

for i in $(seq $base $step $end); do
    if [ $i -lt 10 ]; then
        filename="00"$i
    elif [ $i -lt 100 ]; then
        filename="0"$i
    else
        filename=$i
    fi

    ./ppm-mandelbrot.out $w $h $i > mandelbrots/a$filename.ppm
    ./pgm-mandelbrot.out $w $h $i > mandelbrots/b$filename.pgm
done
