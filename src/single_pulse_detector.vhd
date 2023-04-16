library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity single_pulse_detector is
    generic (
        detect_type : std_logic_vector(1 downto 0) := "00" -- edge detection type
    );
    port (
        clk          : in std_logic;   -- Input clock
        rst          : in std_logic;   -- Asynchronous active high reset
        input_signal : in std_logic;   -- Input signal to detect
        output_pulse : out std_logic); -- Detected single pulse
end single_pulse_detector;

architecture Behavioral of single_pulse_detector is
    signal ff0 : std_logic; -- Input flip-flop
    signal ff1 : std_logic; -- Input flip-flop
begin

    process (clk, rst)
    begin
        if rst = '1' then -- Asynchronous active high reset
            ff0 <= '0';       -- Clear input flipflop 1.
            ff1 <= '0';       -- Clear input flipflop 2.
        elsif rising_edge(clk) then
            ff0 <= input_signal; -- Store input value in 1st ff.
            ff1 <= ff0;          -- Store 1st ff value in 2nd ff.
        end if;
    end process;

    with detect_type select output_pulse <=
        not ff1 and ff0 when "00", -- Detect rising edge
        not ff0 and ff1 when "01", -- Detect falling edge
        ff0 xor ff1 when "10",     -- Detect both edges
        '0' when others;           -- Detect none

end Behavioral;