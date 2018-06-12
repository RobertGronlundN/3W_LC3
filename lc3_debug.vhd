--This is the debug module that allow to single step an LC3 program
-- on the FPGA. It displays on the 7segment display information about
-- the memory interface (data bus, address bus, read enable (RE) and
-- write enable (WE))

Library UNISIM;
use UNISIM.vcomponents.all;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity lc3_debug is
	port (clk : in std_logic;
			cpu_clk_enable : out std_logic;
			address: in std_logic_vector(15 downto 0);
			data: in std_logic_vector(15 downto 0);
			RE, WE: in std_logic;
			btns, btnu, btnl, btnd, btnr: in std_logic;
			sys_reset, sys_program : out std_logic;
			sseg_reg: in std_logic_vector(15 downto 0);
         hex : out std_logic_vector(15 downto 0);
         dot : out std_logic_vector(3 downto 0);
			SW : in std_logic_vector(4 downto 0);
			btn_dbg_io : out std_logic_vector(4 downto 0)
			);		
end lc3_debug;

architecture Structural of lc3_debug is
	signal sys_halt_tick : std_logic;
	signal sys_halted: std_logic;
	signal hand_clk_tick: std_logic;
	signal btnu_tmp,btnd_tmp,btnr_tmp,btns_tmp, btnl_tmp : std_logic;
   signal dbg_btn : std_logic:= '0';

begin

	btnu_tmp <= btnu when dbg_btn='1' else '0';
	btnd_tmp <= btnd when dbg_btn='1' else '0';
	btnr_tmp <= btnr when dbg_btn='1' else '0';
	btns_tmp <= btns when dbg_btn='1' else '0';
	btnl_tmp <= btnl when dbg_btn='1' else '0';

	sys_reset <= btns_tmp;
	sys_program <= btnl_tmp;
   
	--toggling between the debug and io mode for by pressing btnu and btnd simultaniously
	BtnToggle_1: entity work.BtnToggle
	port map(
		btnu => btnu, 
		btnd => btnd, 
		clk => clk,
		dbg_btn => dbg_btn
	);

	-- sys_halt tick is produced every time we press btnr
	sys_halt_db: entity work.debounce(fsmd_arch)
	port map( clk => clk,  reset=>'0', input => btnr_tmp, tick => sys_halt_tick );
	
	--creating the manual clock
	--hand_clk_tick is the tick produced every time we press btnd
	hand_clk_db: entity work.debounce(fsmd_arch)
	port map( clk => clk,  reset=>'0', input => btnd_tmp, tick => hand_clk_tick );


	-- This register flips the value of sys_halted every time btnr is pressed
	process (clk)
	begin
		if clk'event and clk = '1' then
			if sys_halt_tick = '1' then
				sys_halted <= not sys_halted;
			end if;
		end if;
	end process;

	-- Selecting between a manual clock and the system clock
	-- I've disabled the manual clock in case btnu is pressed so that I avoid the clock advancing when
	-- moving between the clk and debug mode for buttons.
	cpu_clk_enable <= hand_clk_tick and not btnu when sys_halted='1' else '1';
	

	--Writing values on the hex display
	hex <= address when sys_halted='1' and btnu_tmp='0' else
					data when sys_halted='1' and btnu_tmp='1' else
					sseg_reg;

	--Controlling the dots from the 7segment display
	dot <= sys_halted & dbg_btn & RE & WE when sys_halted='1' else "0" & dbg_btn & "00";

	--When we single step in the debugging mode then the value for the buttons is actually coming from the switches
	btn_dbg_io <= SW(4 downto 0) when dbg_btn = '1' and sys_halted='1' else btns & btnr & btnd & btnl & btnu;
	
end Structural;

