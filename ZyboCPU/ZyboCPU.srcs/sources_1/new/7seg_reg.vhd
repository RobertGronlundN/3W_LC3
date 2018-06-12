----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/11/2018 01:28:54 PM
-- Design Name: 
-- Module Name: 7seg_reg - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity seg7_reg is
    Port ( rw_en : in STD_LOGIC;
        clk :   in std_logic;
        reg_data_in :   in std_logic_vector(15 to 0);
        sseg :  out std_logic_vector(15 to 0)
    );
        
end seg7_reg;

architecture Behavioral of seg7_reg is

begin
    --process (clk)
      --  begin
            --if (clk'event and clk = '1' and rw_en = '1') then
                sseg <= reg_data_in;                
                --end if;
    -- end process;


end Behavioral;
