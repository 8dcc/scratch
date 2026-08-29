library ieee;
use ieee.std_logic_1164.all;

entity full_adder is
    port (
        a           : in std_logic;
        b           : in std_logic;
        carry_in    : in std_logic;
        sum         : out std_logic;
        carry_out   : out std_logic
    );
end entity;

architecture struct of full_adder is
    -- Define the half-adder component, so we can include them in the full-adder
    component half_adder is
        port (
            a       : in std_logic;
            b       : in std_logic;
            sum     : out std_logic;
            carry   : out std_logic
        );
    end component;

    -- Internal signals used in the full-adder. Used as "buffers" in a way.
    signal sum1, carry1, carry2 : std_logic;
begin
    -- We map (i.e. route) the A and B inputs of the full adder to the first
    -- half-adder. We save the sum and carry of the first half-adder to
    -- intermediate signals we declared in the full-adder above.
    HA1 : half_adder
        port map (
            a => a,
            b => b,
            sum => sum1,
            carry => carry1
        );

    -- We map the sum produced by the first half-adder (in the 'sum1' signal)
    -- to input A of the second half-adder, and the 'carry_in' input of the
    -- full-adder to input B of HA2. The sum produced by HA2 is the sum produced
    -- by the full-adder, but we save the carry to a signal like with HA1.
    HA2 : half_adder
        port map (
            a => sum1,
            b => carry_in,
            sum => sum,
            carry => carry2
        );

    -- To produce the carry of the full-adder, we simply OR the carry signals
    -- produced by the two half-adders.
    carry_out <= carry1 or carry2;
end architecture;
