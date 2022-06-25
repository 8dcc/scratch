## `printlf`

`main.asm` will call `my-include.asm` and use its functions.

The strings in `main.asm` do not have `0xA` at the end ('\n') because the print function we are calling will add it later.
