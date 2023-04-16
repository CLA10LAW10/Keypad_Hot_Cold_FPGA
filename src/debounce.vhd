library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity debounce is
    generic (
        clk_freq    : integer := 125_000_000; --system clock frequency in Hz
        stable_time : integer := 10);         --time button must remain stable in ms
    port (
        clk    : in std_logic;   --input clock
        rst    : in std_logic;   --asynchronous active high reset
        button : in std_logic;   --input signal to be debounced
        result : out std_logic); --debounced signal
end debounce;

architecture Behavioral of debounce is

    signal flipflops   : std_logic_vector (1 downto 0); -- Input flip-flop
    signal counter_set : std_logic;                     -- Sync reset to zero

begin

    counter_set <= flipflops(0) xor flipflops(1); -- Determine when to start/reset counter.

    process (clk, rst)
        variable count : integer range 0 to clk_freq * stable_time / 1000; -- count from 0 to stable debounce time
    begin
        if rst = '1' then             --asynchronous active high reset
            flipflops <= (others => '0'); -- Clear input flipflops.
            result    <= '0';             -- Clear result register.
        elsif rising_edge (clk) then
            flipflops(0) <= button;       -- Store button value in 1st ff.
            flipflops(1) <= flipflops(0); -- Store 1st ff value in 2nd ff.

            if counter_set = '1' then                          -- Used to determine the start/reset of the counter
                count := 0;                                        -- counter_set detected a rising/falling edge, delay/debounce
            elsif (count < clk_freq * stable_time / 1000) then -- If less than the debounce/stable button time
                count := count + 1;                                -- Keep delaying 
            else                                               -- Stable input is met.
                result <= flipflops(1);                            -- Output the stable value.
            end if;
        end if;

    end process;

end Behavioral;