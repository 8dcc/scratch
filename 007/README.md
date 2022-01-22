## `detab.c`
Sample input:
```
	test! one tab
		test! two tabs
	test 1 tab
test 0 tabs

test tab mid line	aaaaa
test tab mid line 2	aaaaa
```
Output with `TAB_REPLACEMENT '-'`:
```
----test! one tab
-------test! two tabs
----test 1 tab
test 0 tabs

test tab mid line---aaaaa
test tab mid line 2-aaaaa
-------------------------
Replaced 7 lines.
```
*Last two lines can be removed commenting a line*

## `entab.c`
Sample input:
```
    test 4 spaces       |
        test 8 spaces   |
    test 4 spaces       |
  test 2 spaces         |

test 7 spaces mid line       ENDOFLINE
test 4 spaces mid line    ENDOFLINE
test 3 spaces mid line   ENDOFLINE
test 2 spaces mid line  ENDOFLINE
```
Output with `TAB_SIZE 4`:
```
	test 4 spaces		|
		test 8 spaces	|
	test 4 spaces		|
  test 2 spaces 		|

test 7 spaces mid line		 ENDOFLINE
test 4 spaces mid line	  ENDOFLINE
test 3 spaces mid line	 ENDOFLINE
test 2 spaces mid line	ENDOFLINE
```
*Have in mind github doesn't use 4 space tabs*

## `fold_lines.c`
Sample input (one line):
```
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 1 2 3 4 dolore magna aliqua.
```
Output with `MAX_OUTPUT_SIZE 40`:
```
Lorem ipsum dolor sit amet, consectetur
adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna
aliqua. Lorem ipsum dolor sit amet,
consectetur adipiscing elit, sed do
eiusmod tempor incididunt ut labore 1 2
3 4 dolore magna aliqua.
```

## `remove_comments.c`
You can remove the comments from the source itself by doing:
```bash
remove_comments.out < remove_comments.c > remove_comments_removed.c
```
You can check the result file [here](https://github.com/r4v10l1/c-stuff/blob/main/007/remove_comments_removed.c).
