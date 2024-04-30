struct SquareIterator {
	arr []int
mut:
	idx int
}

fn main() {
	si := SquareIterator{
		arr: [1,2,3]
		idx: 1
	}
	println(si)
	println(si.arr)
	println(si.idx)
}
