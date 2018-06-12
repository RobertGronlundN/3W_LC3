--This code has been simplified a bit compared with the varions in the **CHU boook

-- Listing 6.1
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity debounce is
   port(
      clk, reset: in std_logic;
      input: in std_logic;
      output: out std_logic;
		tick : out std_logic
   );
end debounce ;


-- Listing 6.2
architecture fsmd_arch of debounce is
   constant N: integer:=21;  -- filter of 2^N * 20ns = 40ms
   type state_type is (zero, wait0, one, wait1);
   signal state_reg, state_next: state_type;
   signal q_reg, q_next: unsigned(N-1 downto 0);
begin
   -- FSMD state & data registers
   process(clk,reset)
   begin
      if reset='1' then
         state_reg <= zero;
         q_reg <= (others=>'0');
      elsif (clk'event and clk='1') then
         state_reg <= state_next;
         q_reg <= q_next;
      end if;
   end process;
   -- next-state logic & data path functional units/routing
   process(state_reg,q_reg,input,q_next)
   begin
      state_next <= state_reg;
      q_next <= q_reg;
		tick <= '0';
      case state_reg is
         when zero =>
            output <= '0';
            if (input='1') then
               state_next <= wait1;
               q_next <= (others=>'1');
            end if;
         when wait1=>
            output <= '0';
            if (input='1') then
               q_next <= q_reg - 1;
               if (q_next=0) then
                  state_next <= one;
						tick <= '1';
               end if;
            else -- input='0'
               state_next <= zero;
            end if;
         when one =>
            output <= '1';
            if (input='0') then
               state_next <= wait0;
               q_next <= (others=>'1');
            end if;
         when wait0=>
            output <= '1';
            if (input='0') then
               q_next <= q_reg - 1;
               if (q_next=0) then
                  state_next <= zero;
               end if;
            else -- input='1'
               state_next <= one;
            end if;
      end case;
   end process;
end fsmd_arch;
