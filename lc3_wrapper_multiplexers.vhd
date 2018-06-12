--This is the version of the LC3 wrapper where theere are two databuses,
--  one input databus andn one output data bus.
--Multiplexers are used to select who is writing to the input data bus
Library UNISIM;
use UNISIM.vcomponents.all;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity lc3_wrapper_multiplexers is
	port (
		clk        : in    std_logic;
		clk_enable : in    std_logic;
		reset      : in    std_logic;
		program    : in    std_logic;
		addr       : out   std_logic_vector(15 downto 0);
		data_in    : in 	 std_logic_vector(15 downto 0);
		data_out   : out 	 std_logic_vector(15 downto 0);
		RE         : out   std_logic;
		WE         : out   std_logic);
end lc3_wrapper_multiplexers;

architecture structural of lc3_wrapper_multiplexers is
	component lc3 is
		port (
			clk        : in    std_logic;
			clk_enable : in    std_logic;
			reset      : in    std_logic;
			program    : in    std_logic;
			addr       : out   std_logic_vector(15 downto 0);
			data_in    : in    std_logic_vector(15 downto 0);
			data_out   : out   std_logic_vector(15 downto 0);
			RE         : out   std_logic;
			WE         : out   std_logic);
	end component lc3;
   
   attribute box_type : string;
	attribute box_type of lc3 : component is "black_box";
   
begin

	lc3_1 : lc3
		port map(
			clk => clk,
			clk_enable => clk_enable,
			reset => reset,
			program => program,
			addr => addr,
			data_in => data_in,
			data_out => data_out,
			RE => RE,
			WE => WE);
end architecture;
	