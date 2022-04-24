# Arrays in V
<!-- Description? -->

<!-- 
# Table of contents:
-->

## General array operations
### Create arrays
```v
a := [1,2,3]        // Need to be same type

// Empty int array
a := []int{}

// Create an array with reserved memory for 20 elements, with 10 starting as 69
a := []int{len: 10, cap: 20, init: 69}
```
### Get elements from arrays
See also [methods](#get-elements-from-arrays-methods).
```v
a := [1,2,3]
assert a[1] == 2
```

## Built-in methods for arrays
See also: [vlib/arrays](https://modules.vlang.io/arrays.html)

### Methods for creating arrays
```v
a_clone := a.clone()    // Returns the array for making another one
a.reverse()             // Creates an array with elements of a in reverse order
a.reverse_in_place()    // Modifies the a array with the elements reversed
```

### Methods for filtering and mapping arrays
```v
a := [1,2,3,4]
even := nums.filter(it % 2 == 0)    // [2, 4]
// Anonymous functions
even_fn := nums.filter(fn (x int) bool {
	return x % 2 == 0
})
```
```v
words := ['hello', 'world']
upper := words.map(it.to_upper())   // ['HELLO', 'WORLD']
upper_fn := words.map(fn (w string) string {
	return w.to_upper()
})
```

### Concatenate the array elements n times
```v
a.repeat(n)
```

### Insert values to an array
```v
a.insert(i, val)            // Insert val at i index (shifts to left)
a.prepend(val)              // Same as a.insert(0, val)
a << 4                      // Insert 4 in the last pos
a.insert(i, [5, 10, 15])    // They can insert several items
```

### Truncate arrays
```v
a.trim(new_len)         // Only if new_len < a.len (does not change cap)
a.clear()               // Same as a.trim(0)
```
Example:
```v 
mut a := [2,4,6,8]
a.trim(2)               // len 2, cap 4
a.clear()               // len 0, cap 4
```

### Delete elements from array
Example:
```v
a.delete(pos)
a.delete_last()         // Does not change cap?
a.delete_many(start_pos, size)
```
```v
mut a := [2,4,6,8,10,12]
a.delete(1)             // [2,6,8,10,12]
a.delete_last()         // [2,6,8,10]
a.delete_many(2,2)      // [2,6]
```

### Get elements from arrays (methods)
```v
a.first()               // Same as a[0]
a.last()                // Same as a[a.len - 1]
a.pop()                 // Returns and removes a.last()
```

<!-- a.join(joiner) -->
