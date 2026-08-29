Full adder
==========

For simulating the testbench, use GHDL and GTKWave:

```bash
rm out.vcd work*.cf
ghdl -a --std=08 src/half_adder.vhd
ghdl -a --std=08 src/full_adder.vhd
ghdl -a --std=08 src/full_adder_tb.vhd
ghdl -e --std=08 full_adder_tb
ghdl -r --std=08 full_adder_tb --vcd=out.vcd --stop-time=1us
gtkwave out.vcd
```

See [02-HalfAdder](../02-HalfAdder) for more information.
