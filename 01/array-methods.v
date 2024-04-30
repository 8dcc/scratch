
fn main() {
	mut a := [2,4,6,8]	// Needs to be mut
	println("${a:-15} -> $a.len $a.cap")
	a.trim(2)
	println("${a:-15} -> $a.len $a.cap")
	a.clear()
	println("${a:-15} -> $a.len $a.cap \n")

	mut a2 := [2,4,6,8,10,12]
	println("${a2:-20} -> $a2.len $a2.cap")
	a2.delete(1)
	println("${a2:-20} -> $a2.len $a2.cap")
	a2.delete_last()
	println("${a2:-20} -> $a2.len $a2.cap")		// Cap not the same as len??
	a2.delete_many(2, 2)		// Keep in mind 2 is the SIZE to be deleted, NOT the end pos
	println("${a2:-20} -> $a2.len $a2.cap")
	a2.prepend([10,20,30])
	println("${a2:-20} -> $a2.len $a2.cap\n")

	mut a3 := [2,4,6,8]
	println("${a3:-15} -> $a3.len $a3.cap")
	println("${a3.reverse():-15} -> $a3.len $a3.cap")
	a3.reverse_in_place()		// Changes the array itself
	println("${a3:-15} -> $a3.len $a3.cap")
	// a3.repeat(1)
	println("${a3.repeat(2):-15} -> $a3.len $a3.cap")
}
