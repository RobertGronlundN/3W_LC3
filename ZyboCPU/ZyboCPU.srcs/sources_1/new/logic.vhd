----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/08/2018 10:18:48 AM
-- Design Name: 
-- Module Name: logic - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity logic is
    Port ( addr : in STD_LOGIC_VECTOR (15 downto 0);
           RE       :   in std_logic;
           WE       :   in std_logic;
           mux_select : out std_logic_vector(3 downto 0);
           rd_uart  :   out std_logic;
           mem_en   :   out std_logic;
           rw_en    :   out std_logic;                      -- Read when 0, write when 1.      
           rx_empty :   in std_logic;                       -- Ready bit is NOT(RX_EMPTY), RX_EMPTY is high when the fifo is empty.
           tx_full  :   in std_logic;                       -- Ready bit is NOT(TX_FULL), TX_FULL is high when the fifo is full.
           rx_rd    :   out std_logic;                      -- Set high when byte is read (at the same time as RE is high)
           tx_wr    :   out std_logic);                     -- Set high when byte is written (at the same time as WE is high)
end logic;

architecture Behavioral of logic is
    
begin
    process (addr)
    begin
        case to_integer(unsigned(addr)) is
        
            when 16#0000# to 16#fdff# => mux_select <= "0000";
                rw_en <= '0';
                mem_en <= '1';
            
            when 16#FE0A# => mux_select <= "0001";
            
            when 16#FE12# => 
                if (WE = '1') then
                    rw_en <= '1';
                end if;
            
            -- STD IN VIRTUAL UART SIGNALS
            when 16#FE00# => mux_select <= "0011";      -- Enables mux output to be the status signal from FIFO STD IN
            when 16#FE02# => 
                if (NOT(rx_empty) = '1' and RE = '1') then
                    rx_rd <= '1';
                    mux_select <= "0010";
                end if;
                
            -- STD OUT VIRTUAL UART SIGNALS
            when 16#FE04# => mux_select <= "0100";      -- Enables mux output to be the status signal from FIFO STD OUT
            when 16#FE06# =>
                if (NOT(tx_full) = '1' and WE = '1') then
                    tx_wr <= '1';
                    mux_select <= "0101";
                end if;
            
            when others =>
                mem_en <= '0';
                mux_select <= "0000";               
        end case;
        
    end process;
    
end Behavioral;
