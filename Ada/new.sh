#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Wrong number of arguments. Expected project name."
    exit 1
fi

project_name=$1

if ! [ -d ./hello_world/ ]; then
    echo "Directory 'hello_world' doesn't exist."
    exit 1
fi

set -x

mkdir ./$project_name
cp ./hello_world/hello_world.adb ./$project_name/$project_name.adb
cp ./hello_world/hello_world.gpr ./$project_name/$project_name.gpr

set +x
