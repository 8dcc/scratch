
fn main() {
	print("[")
	for n := 0; n < 6; n++ {
		print("$n")
	}
	println("]")
	panic("n is too big!")
}
