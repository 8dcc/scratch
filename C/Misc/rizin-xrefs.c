/*
 * This program was made for testing xrefs with rizin. You can do that for the
 * error string with the following commands:
 *
 *   $ rizin rizin-xrefs.out
 *   [0x00001050]> aaa
 *   ...
 *   [0x00001050]> s main
 *   [0x0000115d]> pdf
 *   ...
 *   [0x0000115d]> axl~str.Error
 *       main+21 0x1172 ->      DATA -> 0x2004 str.Error._Exiting.
 *       main+78 0x11ab ->      DATA -> 0x2004 str.Error._Exiting.
 *   [0x0000115d]> pd 3 @ main+21
 *   │   dbg.main+0x15    lea   rax, str.Error._Exiting. ; 0x2004 ; "Error. ...
 *   │   dbg.main+0x1c    mov   rdi, rax ; const char *s
 *   │   dbg.main+0x1f    call  sym.imp.puts ; sym.imp.puts ; int puts(const...
 *   [0x0000115d]> pd 3 @ main+78
 *   │   dbg.main+0x4e    lea   rax, str.Error._Exiting. ; 0x2004 ; "Error. ...
 *   │   dbg.main+0x55    mov   rdi, rax ; const char *s
 *   │   dbg.main+0x58    call  sym.imp.puts ; sym.imp.puts ; int puts(const...
 *
 * Command meaning:
 *   aaa            | Analyze all calls, references, emulation and applies
 *                  | signatures.
 *   s main         | Seek to address.
 *   pdf            | Disassemble a function (the current one in this case)
 *   axl~str.Error  | List all xrefs, and grep "str.Error"
 *   pd 3 @ main+21 | Temporarily seek main+21 and disassemble 3 instructions
 */

#include <stdbool.h>
#include <stdio.h>

static bool test_func(int n) {
    return n % 2 == 0;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        puts("Error. Exiting.");
        return 1;
    }

    /* Is the first char of first argument not even? */
    const char c = argv[1][0];
    if (!test_func(c)) {
        puts("Error. Exiting.");
        return 1;
    }

    printf("Character: %c (%d) is even.\n", c, c);
    puts("Success. Exiting.");

    return 0;
}
