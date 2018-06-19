--Here is the top level of the LC3 computer that can be inmplemented
--  an a Zybo board.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity LC3Zybo_top is
  port (
      clk125 : in std_logic;
      blinky : out std_logic;
      pbtn : in std_logic_vector(3 downto 0);
      psw : in std_logic_vector(3 downto 0);
      pled : out std_logic_vector(2 downto 0);
      
      -- PINOUT FOR UART
      rx     :   in  std_logic;
      tx    :   out std_logic;
      
      -- SPI CLOCK
      spi_clk          : out std_logic;
      spi_status       : out std_logic;
      spi_data         : in std_logic; 
      spi_cs           : out std_logic;
      spi_debug        : out std_logic
  );
end LC3Zybo_top;

architecture Behavioral of LC3Zybo_top is	
   COMPONENT ZyboVIO_SE
	PORT(
		vsw : OUT std_logic_vector(7 downto 0);
		vbtn : OUT std_logic_vector(4 downto 0);          
		vled : IN std_logic_vector(7 downto 0);
      vsseg : IN std_logic_vector (7 downto 0);
      van : IN std_logic_vector (3 downto 0);
      useVHex47Seg : IN std_logic;
		vhex : IN std_logic_vector(15 downto 0);
      sink : IN std_logic;
      blinky : OUT std_logic
		);
	END COMPONENT;
   attribute box_type : string; 
   attribute box_type of ZyboVIO_SE : component is "black_box"; 


   signal clk : std_logic;   
   signal sw : std_logic_vector (7 downto 0);
	signal btn : std_logic_vector(4 downto 0);          
	signal led : std_logic_vector(7 downto 0);
   signal sseg : std_logic_vector(7 downto 0);
   signal an : std_logic_vector(3 downto 0);
	signal hex : std_logic_vector(15 downto 0);
	signal dot : std_logic_vector(3 downto 0);
   signal useHex47Seg : std_logic;
   signal psw_d : std_logic_vector(3 downto 0);
   signal pbtn_d : std_logic_vector(3 downto 0);
   signal sink : std_logic;
	signal rx_data, tx_data : std_logic_vector(7 downto 0);
   signal rx_empty, tx_full : std_logic;
   signal rx_rd, tx_wr : std_logic;
begin

	--Here is an instance of the component where students can write their code
   Inst_student_code: entity work.student_code 
   PORT MAP(
		clk => clk,
		
		btn => btn,
		sw => sw,
		led => led,
		hex => hex,
		dot => dot,
		pbtn => pbtn_d,
		psw => psw_d,
		pled => pled,
        sink => sink,
		
	  rx_data => rx_data,
      rx_rd => rx_rd,
      rx_empty => rx_empty,
      tx_data => tx_data,
      tx_wr => tx_wr,
      tx_full => tx_full,
      
      -- PINOUT FOR UART
      rx => rx,
      tx => tx,		
      
      -- SPI CLOCK
      spi_clk => spi_clk,
      spi_status => spi_status,
      spi_data => spi_data,
      spi_cs => spi_cs,
      spi_debug => spi_debug
	);

	--Here we instantiate the component that allows us to connect to the 
	--  Virtual IO interface from the PC
	Inst_ZyboVIOSE_wrapper: entity work.ZyboVIOSE_wrapper 
   PORT MAP(
      clk125 => clk125,
      clk => clk,

		vled => led,
		vsw => sw,
		vbtn => btn,
		vhex => hex,
		vdot => dot,

      rx_data => rx_data,
      rx_rd => rx_rd,
      rx_empty => rx_empty,
      tx_data => tx_data,
      tx_wr => tx_wr,
      tx_full => tx_full,

		sink => sink,
		blinky => blinky
	);
	
   --Here we debounce the physical buttons and switches
    my_debounce: for i in 0 to 3 generate
      --debounding the physical buttons
      Inst_debounce1: entity work.debounce 
      PORT MAP(
         clk => clk,
         reset => '0',
         input => pbtn(i),
         output => pbtn_d(i)
      );
      --debouncing the physical switches
      Inst_debounce2: entity work.debounce 
      PORT MAP(
         clk => clk,
         reset => '0',
         input => psw(i),
         output => psw_d(i)
      );
   end generate;
end Behavioral;

