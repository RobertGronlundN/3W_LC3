----------------------------------------------------------------------------------
--Here is the top level file where students can write their VHDL code
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity student_code is
    Port ( 
		   clk : in  STD_LOGIC;
			  
		   btn : in  STD_LOGIC_VECTOR (4 downto 0);
           sw : in  STD_LOGIC_VECTOR (7 downto 0);
           led : out  STD_LOGIC_VECTOR (7 downto 0);
           hex : out STD_LOGIC_VECTOR (15 downto 0);
		   dot : out std_logic_vector (3 downto 0);
           pbtn : in  STD_LOGIC_VECTOR (3 downto 0);
           psw : in  STD_LOGIC_VECTOR (3 downto 0);
           pled : out  STD_LOGIC_VECTOR (2 downto 0);
           sink : out STD_LOGIC;

		   rx_data : in std_logic_vector(7 downto 0);
           rx_rd : out std_logic;
           rx_empty : in std_logic;
           tx_data : out std_logic_vector(7 downto 0);
           tx_wr : out std_logic;
           tx_full : in std_logic;
           
           -- PINOUT FOR UART
           rx    : in std_logic;
           tx   : out std_logic;
           
           -- SPI CLOCK
           spi_clk : out std_logic;
           spi_status       : out std_logic;
           spi_data         : in std_logic; 
           spi_cs           : out std_logic
       );
           
end student_code;

architecture Behavioral of student_code is
   --Creating user friently names for the buttons
   alias btn_u : std_logic is btn(0); --Button UP
   alias btn_l : std_logic is btn(1); --Button LEFT
   alias btn_d : std_logic is btn(2); --Button DOWN
   alias btn_r : std_logic is btn(3); --Button RIGHT
   alias btn_s : std_logic is btn(4); --Button SELECT (center button)
   alias btn_c : std_logic is btn(4); --Button CENTER

   
   signal cpu_clk_enable : std_logic;
   signal address: std_logic_vector(15 downto 0);
   signal data: std_logic_vector(15 downto 0);
   signal RE, WE: std_logic;
   signal sys_reset, sys_program : std_logic;
	signal sseg_reg : std_logic_vector (15 downto 0);
	signal btn_io : std_logic_vector(4 downto 0);
   --------------------------------------------------------------------------
   --From here onward you can write your signal and component declarations --
   --------------------------------------------------------------------------
   
   
begin

	--Instance of the LC3 computer
	Inst_lc3_computer: entity work.lc3_computer 
   PORT MAP(
		clk => clk,
		
		led => led,
		btn => btn_io,
		sw =>  sw,
		hex => sseg_reg,
		
		psw => psw,
		pbtn => pbtn,
		pled => pled,
		
		rx_data => rx_data,
		tx_data => tx_data,
        rx_rd => rx_rd,
		rx_empty => rx_empty,
        tx_wr => tx_wr,
		tx_full => tx_full,
		
		sink => sink,
      
		cpu_clk_enable => cpu_clk_enable,
		sys_reset => sys_reset,
		sys_program => sys_program,

		address_dbg => address,
		data_dbg => data,
		RE_dbg => RE,
		WE_dbg => WE,
		
		-- PINOUT FOR UART
		rx => rx,
		tx => tx,
		
		-- SPI CLOCK
		spi_clk => spi_clk,
		spi_status => spi_status,
        spi_data => spi_data,
        spi_cs => spi_cs
	);
	
	--Instance of the debuging module for the LC3 computer
	lc3_debug_1: entity work.lc3_debug
	port map(
			clk => clk,
			cpu_clk_enable => cpu_clk_enable,
			address => address,
			data => data,
			RE => RE, WE => WE,
			btns => btn_s, btnu => btn_u, btnl => btn_l, btnd => btn_d, btnr => btn_r,
			sys_reset => sys_reset, sys_program => sys_program,
			sseg_reg => sseg_reg,
         hex => hex,
         dot => dot,
			sw => sw(4 downto 0),
			btn_dbg_io => btn_io
	);
	
   --------------------------------------------------
   --From here onward you can write your VHDL code.--
   --------------------------------------------------
   
	
	
end Behavioral;

