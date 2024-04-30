/*
Basically what I did with the project folder thing
*/

fn main() {
	println("Calling function add: ${add(1, 2)}")
	println("Calling function sub: ${sub(5, 2)}")
	println("Calling function multple_returns: ${multiple_returns('foo', 'bar')}")
	mtest1, mtest2 := multiple_returns('foo', 'bar')
	println("Printing variables mtest1 and mtest2: $mtest1 $mtest2")
}

fn add(val1 int, val2 int) int {
	return val1 + val2
}

fn sub(val1 int, val2 int) int {
	return val1 - val2
}

fn multiple_returns(val1 string, val2 string) (string, string) {
	return val2, val1
}
