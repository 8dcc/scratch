#!/bin/bash

pid=$(pidof "main.out")
libpath=$(realpath "libtest.so")

# 0x2 -> RTLD_NOW
sudo lldb --batch --attach-pid $pid \
    --one-line "expression (void*)dlopen(\"$libpath\", 0x00002)" \
    --one-line 'print (char*)dlerror()'
