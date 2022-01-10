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
*(Last two lines can be removed commenting a line)*

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
*(Have in mind github doesn't use 4 space tabs)*
