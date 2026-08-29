library ieee;
use ieee.std_logic_1164.all;

-- We need to create an entity for the testbench, even if empty (no I/O)
entity half_adder_tb is
end entity;

architecture sim of half_adder_tb is
    -- Internal "wires" of the testbench
    signal a, b, sum, carry : std_logic;
begin
    -- Create an instance of the 'half_adder'. This can be done after VHDL-93;
    -- before, you had to use 'component'.
    --
    -- The 'port' connects the ports of the 'half_adder' entity to the
    -- testbench signals (same names).
    --
    -- dut = Device Under Test
    dut : entity work.half_adder
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
