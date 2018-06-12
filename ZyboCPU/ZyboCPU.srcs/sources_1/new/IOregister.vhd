----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/11/2018 10:46:29 AM
-- Design Name: 
-- Module Name: IOregister - Behavioral
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

entity SW_reg is
    Port ( sw : in STD_LOGIC_VECTOR (7 downto 0);
           sw_reg_data  :   out std_logic_vector(15 downto 0);           
           clk  :   in  STD_LOGIC);
end SW_reg;

architecture Behavioral of SW_reg is

begin
    process (clk)
        begin
            if (clk'event and clk = '1') then
                sw_reg_data(0) <= sw(0);
                sw_reg_data(1) <= sw(1);
                sw_reg_data(2) <= sw(2);
                sw_reg_data(3) <= sw(3);
                sw_reg_data(4) <= sw(4);
                sw_reg_data(5) <= sw(5);
                sw_reg_data(6) <= sw(6);
                sw_reg_data(7) <= sw(7);
                sw_reg_data(15 downto 8) <= "00000000";
                
                end if;
        end process;
       

end Behavioral;
