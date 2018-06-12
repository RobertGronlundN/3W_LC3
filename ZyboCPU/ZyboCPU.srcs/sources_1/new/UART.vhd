----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/07/2018 12:01:35 PM
-- Design Name: 
-- Module Name: UART - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity UART is
  Port ( 
        u_clk              : in std_logic;
        u_rx_data          : out  std_logic_vector(7 downto 0);
        u_rx_rd            : in std_logic;
        u_rx_empty         : out  std_logic;
        u_tx_data          : in std_logic_vector(7 downto 0);
        u_tx_wr            : out std_logic;
        u_tx_full          : out  std_logic        
     );
     
end UART;

architecture Behavioral of UART is

begin


end Behavioral;
