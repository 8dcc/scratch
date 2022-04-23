
fn main() {
	println("variables.v")
	println("Program for testing and trying the variable types and interactions in V")
	println("General syntax in most cases:")
	println("variable_name -> value -> type")
	/* ----------------------- */
	println("\n------ Variables ------")
	// unused_var_example := 420
	example_var := 123
	println("${'example_var':-20} -> ${example_var:5} -> ${typeof(example_var).name}")
	two_times := example_var * 2
	println("${'two_times':-20} -> ${two_times:5} -> ${typeof(two_times).name}")
	example_float := 10.5
	println("${'example_float':-20} -> ${example_float:5.1} -> ${typeof(example_float).name}")

	println("\n------ Mut variables ------")
	mut mutable_var := example_var / 2
	mutable_var = 6969
	println("${'mutable_var':-20} -> $mutable_var")

	println("\n------ Strings ------")
	mut mut_string_example := "boringCamelCaseString"
	mut_string_example = "I am a mut string example!"
	/*
	>String values are immutable. You cannot mutate elements:
	>  mut s := 'hello world'
	>  s[0] = `H` // not allowed
	>error: cannot assign to s[i] since V strings are immutable

	However, I can do it with unmutable strings (not with runes)...
	*/
	println("${'mut_string_example':-20} -> $mut_string_example")
	mut_string_example += " [Text got appended to me!]"
	println("${'mut_string_example':-20} -> $mut_string_example")
	string_escaped := "   \x41\123\r->\n"		// x41 hex -> 65 dec -> 'A'
	print(string_escaped)	// Note that the \n is from the escaped string

	println("\n------ Runes ------")
	rune_test := `\u6969`	// Random ass chinese character
	println("${'rune_test':-20} -> ${rune_test:-20} -> ${typeof(rune_test).name}")
	rune_test_s := rune_test.str()
	println("${'rune_test_s':-20} -> ${rune_test_s:-20} -> ${typeof(rune_test_s).name}")
	rune_test_b := rune_test.bytes()
	println("${'rune_test_b':-20} -> ${rune_test_b:-20} -> ${typeof(rune_test_b).name}")
	rune_test2 := `a`
	println("${'rune_test2':-20} -> ${rune_test2:-20} -> ${typeof(rune_test2).name}")
	rune_test3 := `\x62`
	println("${'rune_test3':-20} -> ${rune_test3:-20} -> ${typeof(rune_test3).name}")

	println("\n------ Arrays ------")
	mut test_array := [15, 20, 25, 30]
	println("${'test_array':-20} -> ${test_array:-17} -> ${typeof(test_array).name} -> len:${test_array.len} cap:${test_array.cap}")
	test_array = [15]
	println("${'test_array (changed)':-20} -> ${test_array:-17} -> ${typeof(test_array).name} -> len:${test_array.len} cap:${test_array.cap}")
}

