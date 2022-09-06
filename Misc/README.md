# Misc
You can compile the files using the Makefile. Running `make` should show all available options.

#

### `array_slider.c`
Program made because of (what I hope) is my next project, brainfuck interpreter.
Generates an array of 100 strings with len 3 that contain the first 100 numbers in hex format. Then shows the array in what is described in the code as a 'page' format:
    - `SLIDER_LEN` is the number of items per page (in this case 7). Imagine we are dividing the array into pages/slices with this len (In this case we are dividing the array with len 100 into pages with size 7).
    - The variable `slider_count` (inside main) is the 'current page number', and it will be updated depending on user input.
    - Depending on the count, the program shows the items in the described page.

### `encode.c`
Will read a filename, read each character of it and wirte to `encoded.txt` each character with the defined `OFFSET`.
    - [ ] Would be easier to just use pipes (`./encoded.out < input.txt > output.txt`) or use a better method to read the input (backspaces for example).

### `fizzbuzz.c`
Classic fizzbuzz exercise. For more information: [Click me](https://en.wikipedia.org/wiki/Fizz_buzz).

### `getchar_test.c`
Simple program for getting char inputs and clearing after. Useful when making more than one 'Press enter to continue'.

### `ncurses.c`
Program using ncurses for creating a table and filling it with random characters.

### `struct-pointers.c`
For messing with pointers and understanding the different ways of accesing structs.
