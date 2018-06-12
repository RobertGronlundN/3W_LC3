library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity zyboviose_wrapper is
   Port (  vled : in  STD_LOGIC_VECTOR (7 downto 0);
           vsw : out STD_LOGIC_VECTOR (7 downto 0);
           vbtn : out std_logic_vector (4 downto 0);
           vhex : in std_logic_vector (15 downto 0);
			  vdot : in std_logic_vector (3 downto 0);
           clk125 : in std_logic;
           clk : out std_logic;
           rx_data : out std_logic_vector(7 downto 0);
           rx_rd : in std_logic;
           rx_empty : out std_logic;
           tx_data : in std_logic_vector(7 downto 0);
           tx_wr : in std_logic;
           tx_full : out std_logic;
			  sink : in std_logic;
           blinky : out std_logic
           );
end zyboviose_wrapper;

architecture Behavioral of zyboviose_wrapper is
   component ZyboVIO_SE is
   Port (  vled : in  STD_LOGIC_VECTOR (7 downto 0);
           vsw : out STD_LOGIC_VECTOR (7 downto 0);
           vbtn : out std_logic_vector (4 downto 0);
           vsseg : in std_logic_vector (7 downto 0);
           van : in std_logic_vector( 3 downto 0);
           useVHex47Seg : in std_logic;
           vhex : in std_logic_vector (15 downto 0);
			  vdot : in std_logic_vector (3 downto 0);
           clk125 : in std_logic;
           clk : out std_logic;
           rx_data_out : out std_logic_vector(7 downto 0);
           rx_rd : in std_logic;
           rx_empty : out std_logic;
           tx_data_in : in std_logic_vector(7 downto 0);
           tx_wr : in std_logic;
           tx_full : out std_logic;
			  sink : in std_logic;
           blinky : out std_logic
           );
   end component;
   
	attribute box_type : string;
	attribute box_type of ZyboVIO_SE : component is "black_box";
begin
	Inst_ZyboVIO_SE: ZyboVIO_SE PORT MAP(
      clk125 => clk125,
      clk => clk,
		vled => vled,
		vsw => vsw,
		vbtn => vbtn,
		vsseg => X"FF",
		van => X"F",
      useVHex47Seg => '1',
		vhex => vhex,
		vdot => vdot,
      rx_data_out => rx_data,
      rx_rd => rx_rd,
      rx_empty => rx_empty,
      tx_data_in => tx_data,
      tx_wr => tx_wr,
      tx_full => tx_full,
		sink => sink,
		blinky => blinky
	);
end Behavioral;

