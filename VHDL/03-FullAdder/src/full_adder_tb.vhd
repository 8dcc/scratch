library ieee;
use ieee.std_logic_1164.all;

entity full_adder_tb is
end entity;

architecture sim of full_adder_tb is
    component full_adder is
        port (
            a           : in std_logic;
            b           : in std_logic;
            carry_in    : in std_logic;
            sum         : out std_logic;
            carry_out   : out std_logic
        );
    end component;

    signal a, b, carry_in, sum, carry_out : std_logic;
begin
    dut : full_adder
        port map (
            a => a,
            b => b,
            carry_in => carry_in,
            sum => sum,
            carry_out => carry_out
        );

    process
    begin
        a <= '0'; b <= '0'; carry_in <= '0'; wait for 10 ns;
        a <= '0'; b <= '0'; carry_in <= '1'; wait for 10 ns;
        a <= '0'; b <= '1'; carry_in <= '0'; wait for 10 ns;
        a <= '0'; b <= '1'; carry_in <= '1'; wait for 10 ns;
        a <= '1'; b <= '0'; carry_in <= '0'; wait for 10 ns;
        a <= '1'; b <= '0'; carry_in <= '1'; wait for 10 ns;
        a <= '1'; b <= '1'; carry_in <= '0'; wait for 10 ns;
        a <= '1'; b <= '1'; carry_in <= '1'; wait for 10 ns;
        wait;
    end process;
end architecture;
