/* https://the-book-of-v.readthedocs.io/en/latest/chapter-1-calculator.html */
import os

fn main() {
	debug := false
	debug_print(debug, "args: $os.args")

	prioritized := ["mul", "div"]
	normal := ["add", "sub"]

	mut	any_operation := normal.clone()
	any_operation << prioritized

	// Storing values and operations 
	mut stack := []f32{}
	mut operation := ""

	for idx, value in os.args {
		if idx == 0 {
			continue	// Skip first argument (filename)
		}

		if value in any_operation {
			debug_print(debug, "Value $value in arrays")
			if operation == "" {
				operation = value
			}
		} else if value !in prioritized && value !in normal {
			debug_print(debug, "Inserting $value to stack...")
			stack << value.f32()
		}
	}

	if stack.len < 2 {
		debug_print(debug, "Small stack len -> $stack")
		panic("Not enough values")
	}

	if operation == '' {
		debug_print(debug, "No operation -> $stack")
		panic("No valid operation!")
	}

	debug_print(debug, "Calculating $operation with: $stack")

	n2 := stack[stack.len - 1]		// n2 first because its the last one
	stack.delete(stack.len - 1)
	n1 := stack[stack.len - 1]
	stack.delete(stack.len - 1)

	// Replace with switch case? (equivalent)
	if operation == "add" {
		debug_print(debug, "Adding $n1 to $n2")
		println("${add(n1, n2):.5f}")
	} else if operation == "sub" {
		debug_print(debug, "Subtracting $n1 from $n2")
		println("${sub(n1, n2):.5f}")
	} else if operation == "mul" {
		debug_print(debug, "Multiplying $n1 to $n2")
		println("${mul(n1, n2):.5f}")
	} else if operation == "div" {
		debug_print(debug, "Dividing $n1 to $n2")
		println("${div(n1, n2):.5f}")
	} else {
		debug_print(debug, "${'n1':-3} -> [$n1]")
		debug_print(debug, "${'n2':-3} -> [$n2]")
		debug_print(debug, "${'operation':-3} -> [$operation]")
		debug_print(debug, "${'stack':-3} -> $stack")
		panic("This should not happen!")
	}
}
