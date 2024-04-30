fn main() {
	println("------ Format specifiers ------")	// <3
	println("${mutable_var:7}")		// Output: '   6969'
	println("${mutable_var:07}")	// Output: '0006969'
	println("${mutable_var:-7}")	// Output: '6969   '
	// A way of using '${foo:-07}' ?
	smol_str := "test"
	println("${smol_str:7}")		// Output: '   test'
	println("${smol_str:7s}")		// Same as before
	println("${smol_str:-7}")		// Output: 'test   '
}
