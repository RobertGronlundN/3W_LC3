--This component allows to toggle the output value (from 1 to 0 or 0 to 1),
-- every time the two input buttons are pressed for a certain time duration.
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity BtnToggle is
	port(
		btnu, btnd, clk : in std_logic;
		dbg_btn : out std_logic
	);
end BtnToggle;

architecture Behavioral of BtnToggle is
	signal counter_next, counter : std_logic_vector(29 downto 0):= (others=>'0');
	signal dbg_btn_reg : std_logic := '1';
begin
	
	count_reg: process(clk, btnu, btnd)
	begin
		if (btnu='0') or (btnd='0') then
			counter <= (others => '0');
		elsif rising_edge(clk) then
			counter <= counter_next;
		end if;	
	end process;
	
	counter_next <= counter + 1;
	
	dbg_btn_register_1: process(clk)
	begin
		if rising_edge(clk) then
				if counter = "00" & X"2000000" then -- a bit over 1 sec for a 50Mhz clk
				dbg_btn_reg <= not dbg_btn_reg;
			end if;
		end if;
	end process;
	
	dbg_btn <= dbg_btn_reg;

end Behavioral;

