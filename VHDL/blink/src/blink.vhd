library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blink is
    port (
        clk : in  std_logic;
        led : out std_logic_vector(5 downto 0)
    );
end entity;

architecture rtl of blink is
    signal counter : unsigned(23 downto 0) := (others => '0');
    signal led_reg : std_logic := '1'; -- '1' = off (active low)
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if counter = 13_499_999 then
                counter <= (others => '0');
                led_reg <= not led_reg;
            else
                counter <= counter + 1;
            end if;
        end if;
    end process;

    led <= (others => led_reg); -- drive all 6 LEDs together
end architecture;
