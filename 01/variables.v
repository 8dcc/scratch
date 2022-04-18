
fn main() {
	println("\n------ Variables ------")
	// unused_var_example := 420
	example_var := 123
	println("example_var    -> $example_var")
	two_times := example_var * 2
	println("two_times      -> $two_times")

	println("\n------ Mut variables ------")
	mut mutable_var := example_var / 2
	mutable_var = 6969
	println("mutable_var    -> $mutable_var")

	println("\n------ Strings ------")
	mut mut_string_example := "boringCamelCaseString"
	mut_string_example = "I am a mut string example!"
	/*
	>String values are immutable. You cannot mutate elements:
	>  mut s := 'hello 🌎'
	>  s[0] = `H` // not allowed
	>error: cannot assign to s[i] since V strings are immutable

	However, I can do it with strings (not with runes)...
	*/
	println("mut_string_example         -> $mut_string_example")
	string_escaped := "   \x41\123\r->\n"		// x41 hex -> 65 dec -> 'A'
	print(string_escaped)

	println("\n------ Conversion tests ------")
	string_n_example := "100"
	int_from_string := string_n_example.int() + 1
	println("int_from_string (+1)       -> $int_from_string")
	println("int_conversion_test (f)    -> ${int_conversion_test(int(3.5))}")
}

fn int_conversion_test(n int) bool {
	return n < 4
}
