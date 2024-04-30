
string_test := "test"
arr := string_test.bytes()

println(arr)
println(arr[0])
println(arr[0].ascii_str())

assert string_test[0] == 116
assert string_test[0].ascii_str() == 't'

println("---------------")
country := 'Netherlands'
println(typeof(country[0]).name) // Output: 78
assert typeof(country[0]).name == 'u8'
println(typeof(country[0].ascii_str()).name) // Output: N
assert typeof(country[0].ascii_str()) == 'string'
