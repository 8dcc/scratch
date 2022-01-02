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
(Last two lines can be removed commenting a line)
