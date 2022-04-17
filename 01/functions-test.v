/*
Basically what I did with the project folder thing
*/

fn main() {
	print("Calling function add: ")
	println(add(5, 2))
	print("Calling function sub: ")
	println(sub(5, 2))
}

fn add(val1 int, val2 int) int {
	return val1 + val2
}

fn sub(val1 int, val2 int) int {
	return val1 - val2
}
