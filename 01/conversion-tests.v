fn main() {
	println("------ Conversion tests ------")
	string_n_example := "100"
	int_from_string := string_n_example.int() + 1
	println("${'int_from_string (+1)':-25} -> $int_from_string")
	temp_conversion_i := int(3.5)
	conversion_check_bool := int_conversion_test(int(3.5))
	println("${'int_conversion_test':-25} -> ${temp_conversion_i} (${typeof(temp_conversion_i).name}) < 4 ->  ${conversion_check_bool}")
}

fn int_conversion_test(n int) bool {
	return n < 4
}
