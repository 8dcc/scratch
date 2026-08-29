library ieee;
use ieee.std_logic_1164.all;

-- We need to create an entity for the testbench, even if empty (no I/O)
entity half_adder_tb is
end entity;

architecture sim of half_adder_tb is

    -- Definition of the 'half_adder' component, identical to the entity in
    -- 'half_adder.vhd'.
    --
    -- After VHDL-93, this can be done in a shorter way, but I prefer this
    -- one for now.
    component half_adder
        port (
            a     : in  std_logic;
            b     : in  std_logic;
            sum   : out std_logic;
            carry : out std_logic
        );
    end component;

    -- Internal "wires" of the testbench
    signal a, b, sum, carry : std_logic;
begin
    -- Create an instance of the 'half_adder' component.
    --
    -- The 'port' connects the ports of the 'half_adder' entity to the
    -- testbench signals (same names).
    --
    -- dut = Device Under Test
    dut : half_adder
        port map (a => a, b => b, sum => sum, carry => carry);

    process
    begin
        a <= '0'; b <= '0'; wait for 10 ns;
        a <= '0'; b <= '1'; wait for 10 ns;
        a <= '1'; b <= '0'; wait for 10 ns;
        a <= '1'; b <= '1'; wait for 10 ns;
        wait;
    end process;
end architecture;
