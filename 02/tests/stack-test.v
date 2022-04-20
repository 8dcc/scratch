
fn main() {
	mut stack := []f32
	stack << 1.1
	stack << 1.2
	mut stack2 := [2.1, 2.2]

	println("$stack (${typeof(stack).name}) -> $stack.len")
	println("$stack2 (${typeof(stack2).name}) -> $stack2.len")
}
