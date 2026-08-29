Half adder
==========

For simulating the testbench, use GHDL and GTKWave:

```bash
# Analyse the entity/architecture, then the testbench. Stores the resulting
# design unit in the work library. Roughly equivalent to compiling.
ghdl -a --std=08 half_adder.vhd
ghdl -a --std=08 half_adder_tb.vhd

# Elaborate. Binds components to entities, connects ports, and builds an
# executable simulation model. Roughly equivalent to linking.
ghdl -e --std=08 half_adder_tb

# Actually executes that elaborated simulation, advancing simulated time and
# evaluating all processes and signal assignments.
ghdl -r --std=08 half_adder_tb --vcd=out.vcd --stop-time=1us

# Visualize results of simulation.
gtkwave out.vcd
```
