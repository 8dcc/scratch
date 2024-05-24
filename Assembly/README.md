Scratch assembly
----------------

Some assembly programs that I have written throughout these years.

Most of them are for the x86 architecture, and are written in NASM syntax. Most
of them have only been tested on GNU/Linux, and some of them use Linux syscalls,
although some others call LIBC functions.

As mentioned in the C folder, the quality of this code depends on when it was
created. I kept old/bad versions so I can see my progression.

Assembling and linking
----------------------

Most sub-folders contain a Makefile, but you might need to assemble some of them
manually. Generally, you just need to assemble with:

    # Change to elf32 depending on the architecture
    nasm -f elf64 -o output.o input.asm

And link it with either:

    # Change to elf_i386 depending on the architecture
    ld -m elf_x86_64 -o output.out output.o

Or:

    # If the assembly depends on LIBC
    gcc -o output.out output.o

Other resources
---------------

Interesting resources related to assembly:
 - http://asm.sourceforge.net
 - http://asm.sourceforge.net/howto/hello.html
 - https://asmtutor.com

About linux syscalls:
 - http://www.lxhp.in-berlin.de/lhpsyscal.html
 - https://linuxhint.com/list_of_linux_syscalls
