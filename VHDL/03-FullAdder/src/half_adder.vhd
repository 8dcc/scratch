library ieee;
use ieee.std_logic_1164.all;

-- For more information and comments, see '../02-HalfAdder'. 
-- This file is unchanged.

entity half_adder is
    port (
        a       : in std_logic;
        b       : in std_logic;
        sum     : out std_logic;
        carry   : out std_logic
    );
end entity;

architecture rtl of half_adder is
begin
    sum <= a xor b;
    carry <= a and b;
end architecture;