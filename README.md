# ASM stuff
**Stuff I made while learning assembly**

### Description
Simple assembly files I used for learning. Most of them have only been tested on
linux and/or use linux syscalls, so keep that in mind.

Just like in [`c-stuff`](https://github.com/8dcc/c-stuff) you will be able to
see my progression. And that might include bad practices at first.

### Building
You can use my small
[`nasmo`](https://github.com/8dcc/linux-dotfiles/blob/main/scripts/usr/nasmo)
script for quick nasm compile and link, or use the makefile like this:
```console
$ ls misc/11-xchg
simple-lib.asm  xchg.asm

$ make xchg.o     # Optional. Done automatic in .out
$ make xchg.out   # Link object

$ ls misc/11-xchg
simple-lib.asm  xchg.asm  xchg.o  xchg.out

$ ./misc/11-xchg/xchg.out
...
```

### More information 

For more information about assembly:
  - http://asm.sourceforge.net
  - http://asm.sourceforge.net/howto/hello.html
  - https://asmtutor.com

For more information about linux syscalls:
  - http://www.lxhp.in-berlin.de/lhpsyscal.html
  - https://linuxhint.com/list_of_linux_syscalls
