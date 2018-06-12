--This is the version of the LC3 wrapper where the databus is bidirectional
--Trisattes are necessary when writing data to be bus in order to avoid
--  having multiple drivers in the same time.
Library UNISIM;
use UNISIM.vcomponents.all;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity lc3_wrapper_tristates is
	port (
		clk        : in    std_logic;
		clk_enable : in    std_logic;
		reset      : in    std_logic;
		program    : in    std_logic;
		addr       : out   std_logic_vector(15 downto 0);
		data       : inout std_logic_vector(15 downto 0);
		RE         : out   std_logic;
		WE         : out   std_logic);
end lc3_wrapper_tristates;

architecture structural of lc3_wrapper_tristates is
	component lc3 is
		port (
			clk        : in    std_logic;
			clk_enable : in    std_logic;
			reset      : in    std_logic;
			program    : in    std_logic;
			addr       : out   std_logic_vector(15 downto 0);
			data_in    : in std_logic_vector(15 downto 0);
			data_out   : out std_logic_vector(15 downto 0);
			RE         : out   std_logic;
			WE         : out   std_logic);
	end component lc3;
   
   attribute box_type : string;
	attribute box_type of lc3 : component is "black_box";
   
	signal data_out  : std_logic_vector(15 downto 0);
	signal WE1 : std_logic;
begin

	lc3_1 : lc3
		port map(
			clk => clk,
			clk_enable => clk_enable,
			reset => reset,
			program => program,
			addr => addr,
			data_in => data,
			data_out => data_out,
			RE => RE,
			WE => WE1);

	data <= data_out when WE1 = '1' else (others => 'Z');
	WE <= WE1;
end architecture;
	