library ieee;
use ieee.std_logic_1164.all;

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
    -- | A | B | Out | Carry |
    -- |---|---|-----|-------|
    -- | 0 | 0 | 0   | 0     |
    -- | 1 | 0 | 1   | 0     |
    -- | 0 | 1 | 1   | 0     |
    -- | 1 | 1 | 0   | 1     |
    sum <= a xor b;
    carry <= a and b;
end architecture;