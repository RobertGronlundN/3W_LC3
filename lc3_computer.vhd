-- This is the component that you'll need to fill in in order to create the LC3 computer.
-- It is FPGA independent. It can be used without any changes between the Zybo and the 
-- Nexys3 boards.
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity lc3_computer is
   port (
	  -- System clock
      clk              : in  std_logic; 

      -- Virtual I/O
      led              : out std_logic_vector(7 downto 0);
      btn              : in  std_logic_vector(4 downto 0);
      sw               : in  std_logic_vector(7 downto 0);
      hex              : out std_logic_vector(15 downto 0); --16 bit hexadecimal value (shown on 7-seg sisplay)

	  -- Physical I/0 (IO on the Zybo FPGA)
	  pbtn			  : in  std_logic_vector(3 downto 0);
	  psw				  : in  std_logic_vector(3 downto 0);
	  pled				  : out  std_logic_vector(2 downto 0);

	  -- VIO serial
	  rx_data          : in  std_logic_vector(7 downto 0);
      rx_rd            : out std_logic;
      rx_empty         : in  std_logic;
      tx_data          : out std_logic_vector(7 downto 0);
      tx_wr            : out std_logic;
      tx_full          : in  std_logic;
		
	  sink             : out std_logic;

      -- Debug
      address_dbg      : out std_logic_vector(15 downto 0);
      data_dbg         : out std_logic_vector(15 downto 0);
      RE_dbg           : out std_logic;
      WE_dbg           : out std_logic;
		
	  -- LC3 CPU inputs
      cpu_clk_enable   : in  std_logic;
      sys_reset        : in  std_logic;
      sys_program      : in  std_logic;
      
      -- PINOUT FOR UART
      rx               : in  std_logic;
      tx               : out std_logic;
      
      -- SPI CLOCK
      SPI_clk          : out std_logic;
      spi_status       : out std_logic;
      spi_data         : in std_logic;
      spi_cs           : out std_logic;   
      spi_debug        : out std_logic   
   );
end lc3_computer;

architecture Behavioral of lc3_computer is
---------------------------------------------------------------------------------------
-- PREGENERATE CODE                 ---------------------------------------------------
--------------------------------------------------------------------------------------- 
	-- Making	sure	that	our	output	signals	are	not	merged/removed	during	
	-- synthesis. We	achieve	this	by	setting	the keep	attribute for	all	our	outputs
	-- It's good to uncomment the following attributs if you get some errors with multiple 
	-- drivers for a signal.
--	attribute	keep:string;
--	attribute	keep	of	led			: signal	is	"true";
--	attribute	keep	of	pled			: signal	is	"true";
--	attribute	keep	of	hex			: signal	is	"true";
--	attribute	keep	of	rx_rd			: signal	is	"true";
--	attribute	keep	of	tx_data		: signal	is	"true";
--	attribute	keep	of	tx_wr			: signal	is	"true";
--	attribute	keep	of	address_dbg	: signal	is	"true";
--	attribute	keep	of	data_dbg		: signal	is	"true";
--	attribute	keep	of	RE_dbg		: signal	is	"true";
--	attribute	keep	of	WE_dbg		: signal	is	"true";
--	attribute	keep	of	sink			: signal	is	"true";

    -- Creating user friently names for the buttons
    alias btn_u : std_logic is btn(0); --Button UP
    alias btn_l : std_logic is btn(1); --Button LEFT
    alias btn_d : std_logic is btn(2); --Button DOWN
    alias btn_r : std_logic is btn(3); --Button RIGHT
    alias btn_s : std_logic is btn(4); --Button SELECT (center button)
    alias btn_c : std_logic is btn(4); --Button CENTER
   
    signal sink_sw : std_logic;
    signal sink_psw : std_logic;
    signal sink_btn : std_logic;
    signal sink_pbtn : std_logic;
    signal sink_uart : std_logic;
    signal p_sink_uart : std_logic;         -- Physical uart sink (same as above)
   
	-- Memory interface signals
	signal address: std_logic_vector(15 downto 0);
	signal data, data_in, data_out: std_logic_vector(15 downto 0); -- data inputs
	signal RE, WE:  std_logic;

	-- I/O constants for addr from 0xFE00 to 0xFFFF:
    constant STDIN_S    :   std_logic_vector(15 downto 0) := X"FE00";  -- Serial IN (terminal keyboard)
    constant STDIN_D    :   std_logic_vector(15 downto 0) := X"FE02";
    constant STDOUT_S   :   std_logic_vector(15 downto 0) := X"FE04";  -- Serial OUT (terminal  display)
    constant STDOUT_D   :   std_logic_vector(15 downto 0) := X"FE06";
    constant IO_SW      :   std_logic_vector(15 downto 0) := X"FE0A";  -- Switches
    constant IO_PSW     :   std_logic_vector(15 downto 0) := X"FE0B";  -- Physical Switches	
    constant IO_BTN     :   std_logic_vector(15 downto 0) := X"FE0e";  -- Buttons
    constant IO_PBTN    :   std_logic_vector(15 downto 0) := X"FE0F";  -- Physical Buttons	
	constant IO_SSEG    :   std_logic_vector(15 downto 0) := X"FE12";  -- 7 segment
	constant IO_LED     :   std_logic_vector(15 downto 0) := X"FE16";  -- Leds
	constant IO_PLED    :   std_logic_vector(15 downto 0) := X"FE17";  -- Physical Leds
   
    -- I/O for physical UART
    constant P_STDIN_S    :   std_logic_vector(15 downto 0) := X"FE20";  -- Serial IN (terminal keyboard)
    constant P_STDIN_D    :   std_logic_vector(15 downto 0) := X"FE22";
    constant P_STDOUT_S   :   std_logic_vector(15 downto 0) := X"FE24";  -- Serial OUT (terminal  display)
    constant P_STDOUT_D   :   std_logic_vector(15 downto 0) := X"FE26";
    
    -- I/O for SPI
    constant SPI_REG      :   std_logic_vector(15 downto 0) := X"FE34";   
   
    -- MUX signals
    signal ram_out      :   std_logic_vector(15 downto 0);
    signal sw_reg       :   std_logic_vector(15 downto 0);
   
    -- UART signals
    signal p_rx_data    :   std_logic_vector(7 downto 0);
    signal p_rx_rd      :   std_logic;
    signal p_rx_empty   :   std_logic;
    signal p_tx_data    :   std_logic_vector(7 downto 0);
    signal p_tx_wr      :   std_logic;
    signal p_tx_full    :   std_logic;
                     
    signal reset        :   std_logic;
   
    -- logic signals
    signal mux_select   :   std_logic_vector(3 downto 0);    
    signal mem_en       :   std_logic;
    --signal rw_en        :   std_logic;
    
    -- SPI COUNTER SIGNALS
    signal counter          :   std_logic_vector(7 downto 0);
    signal spi_clk_signal   :   std_logic;
    signal tick             :   std_logic;
    signal tick_high        :   std_logic;
    signal tick_low         :   std_logic;    
    
    -- SPI STATE SIGNALS
    signal spi_state        :   std_logic_vector(7 downto 0);
    signal spi_next_state   :   std_logic_vector(7 downto 0);    
    signal ready            :   std_logic;
    
    signal CS               :   std_logic;    
    signal spi_out          :   std_logic;
    signal spi_in           :   std_logic;
    signal spi_temp         :   std_logic_vector(7 downto 0);
    signal spi_mux_I        :   std_logic_vector(15 downto 0);
    signal spi_mux_II       :   std_logic_vector(15 downto 0);
    signal input_select     :   std_logic;
    signal debug            :   std_logic;
	
begin
---------------------------------------------------------------------------------------
-- PREGENERATE CODE                 ---------------------------------------------------
---------------------------------------------------------------------------------------
    -- In order to avoid warnings or errors all outputs should be assigned a value. 
    -- The VHDL lines below assign a value to each otput signal. An otput signal can have
    -- only one driver, so each otput signal that you plan to use in your own VHDL code
    -- should be commented out in the lines below 

    --Virtual Leds on Zybo VIO (active high)
    led(0) <= '0';
    led(1) <= '0';
    led(2) <= '0'; 
    led(3) <= '0'; 
    led(4) <= '0'; 
    led(5) <= '0'; 
    led(6) <= '0'; 
    led(7) <= '0'; 

    --Physical leds on the Zybo board (active high)
    pled(0) <= NOT p_rx_empty;
    pled(1) <= NOT p_tx_full;
    pled(2) <= input_select;
   
    -- All the input signals comming to the FPGA should be used at least once otherwise we get 
    -- synthesis warnings. The following lines of VHDL code are meant to remove those warnings. 
    -- Sink is just an output signal that that has the only purpose to allow all the inputs to 
    -- be used at least once, by orring them and assigning the resulting the value to sink.
    -- You are not suppoosed to modify the following lines of VHDL code, where inputs are orred and
    -- assigned to the sink. 
   
    sink_psw <= psw(0) or psw(1) or psw(2) or psw(3);
    sink_pbtn <= pbtn(0) or pbtn(1) or pbtn(2) or pbtn(3);
    sink_sw <= sw(0) or sw(1) or sw(2) or sw(3) or sw(4) or sw(5) or sw(6) or sw(7); 
    sink_btn <= btn(0) or btn(1) or btn(2) or btn(3) or btn(4);
    sink_uart <= rx_data(0) or rx_data(1) or rx_data(2) or rx_data(3) or rx_data(4) or  rx_data(5) or rx_data(6) or rx_data(7)or rx_empty or tx_full; 
    p_sink_uart <= p_rx_data(0) or p_rx_data(1) or p_rx_data(2) or p_rx_data(3) or p_rx_data(4) or  p_rx_data(5) or p_rx_data(6) or p_rx_data(7)or p_rx_empty or p_tx_full; 
    sink <= sink_sw or sink_psw or sink_btn or sink_pbtn or sink_uart or p_sink_uart;    -- WE HAVE ADDED P_SINK_UART
   
    reset <= '0';
    
---------------------------------------------------------------------------------------
-- END OF PREGENERATE CODE          ---------------------------------------------------
---------------------------------------------------------------------------------------
	--You'll have to decide which type of data bus you need to use for the
	--  LC3 processor. Here are the options:
	-- 1. Bidirectional data bus (to which you write using tristates).
	-- 2. Two unidirctional busses data_in and data_out where you use
	--    multiplexors to dicide where the data for data_in is comming for.
	--The VHDL code that instantiate either one of these options for the LC3
	--  processor are provided below. Just uncomment the one you prefer
	
	-- <<< LC3 CPU using multiplexers for the data bus>>>	
	lc3_mux: entity work.lc3_wrapper_multiplexers
	port map (
		 clk        => clk,
		 clk_enable => cpu_clk_enable,
		 reset      => sys_reset,
		 program    => sys_program,
		 addr       => address,
		 data_in    => data_in,
		 data_out   => data_out,
		 WE         => WE,
		 RE         => RE 
		 );
    data_dbg <= data_in when RE='1' else data_out;    
	-- <<< LC3 CPU using multiplexers end of instantiation>>>	
	
	--Information that is sent to the debugging module
   address_dbg <= address;
   RE_dbg <= RE;
   WE_dbg <= WE;
 
---------------------------------------------------------------------------------------  
-- MEMORY                           ---------------------------------------------------       
---------------------------------------------------------------------------------------  

    lc3_mem: entity work.memory
    port map (
        clk     =>  clk,
        RE      =>  RE,
        WE      =>  WE,
        addr    =>  address,
        din     =>  data_out,
        dout    =>  ram_out,
        mem_en  =>  mem_en
        );

---------------------------------------------------------------------------------------  
-- PHYSICAL UART                    ---------------------------------------------------       
---------------------------------------------------------------------------------------  
        
    lc3_uart: entity work.uart
    port map (
        clk     =>  clk,
        reset   =>  reset,
        rd_uart =>  p_rx_rd,
        wr_uart =>  p_tx_wr,
        r_data  =>  p_rx_data,
        w_data  =>  p_tx_data,
        rx      =>  rx,             -- Naar ledning inkluderes, tilslut pin
        tx      =>  tx,             -- Naar ledning inkluderes, tilslut pin
        tx_full =>  p_tx_full,
        rx_empty=>  p_rx_empty        
        );
    
---------------------------------------------------------------------------------------  
-- TICK CLOCK                       ---------------------------------------------------
--------------------------------------------------------------------------------------- 
   spi_clk <= spi_clk_signal;
   
   process(clk, SPI_clk_signal)
    begin
        if(clk'event and clk = '1') then
            if(counter = X"19") then
                if(SPI_clk_signal = '1') then
                    SPI_clk <= '0';
                    tick_LOW <= '1';
                else
                    SPI_clk <= '1';
                    tick_HIGH <= '1';
                end if;
                counter <= X"00";
            else
                tick_HIGH <= '0';
                tick_LOW <= '0';
                counter <= std_logic_vector(unsigned(counter) + 1);
            end if;
        end if;    
    end process;   
   
---------------------------------------------------------------------------------------
-- SPI                              ---------------------------------------------------
---------------------------------------------------------------------------------------    
--   spi_in       <= spi_data; 
--   spi_status   <= spi_out;  
--   spi_cs       <= CS;
--   spi_debug    <= spi_state(0);
       
   process (clk, ready, spi_next_state)
   begin
        if rising_edge(clk) then
            if ready = '1' then
                spi_state <= spi_next_state;
                ready <= '0';
            else
                ready <= '1';                  
            end if;
        end if;            
   end process;
      
   process (spi_state, tick_high, tick_low, cs, spi_in, spi_next_state, input_select)
    begin      
        spi_in       <= spi_data; 
        spi_status   <= spi_out;  
        spi_cs       <= CS;
        spi_debug    <= spi_state(0);    
          
        case (spi_state) is
            when X"00" =>           -- tSCH
                CS <= '1';
                spi_out <= '0';
                if (tick_low = '1') then
                    spi_next_state <= X"01";
                end if;
             
            when X"01" =>           -- START
               CS <= '1';
               spi_out <= '0';
               if (tick_low = '1') then
                   spi_next_state <= X"02";
               end if;      
            
            when X"02" =>           -- SGL
                CS <= '0';
                spi_out <= '1';
                if (tick_low = '1') then                   
                   spi_next_state <= X"03";
                end if; 
               
            when X"03" =>           -- D2
                CS <= '0';
                spi_out <= '1';
                if (tick_low = '1') then                      
                   spi_next_state <= X"04";
                end if; 
            
            when X"04" =>           -- D1
                CS <= '0';
                spi_out <= '0';
                if (tick_low = '1') then                    
                   spi_next_state <= X"05";
                end if; 
                
            when X"05" =>           -- D0
                CS <= '0';
                spi_out <= '0';
                if (tick_low = '1') then 
                    --spi_out <= '1';
                    if input_select = '1' then spi_out <= '1'; else spi_out <= '0'; end if;
                    spi_next_state <= X"06";
                end if; 
            
            when X"06" =>           -- T SAMPLE I
                CS <= '0';
                --spi_out <= '1';
                if input_select = '1' then spi_out <= '1'; else spi_out <= '0'; end if;
                if (tick_low = '1') then                     
                   spi_next_state <= X"07";
                end if; 
            
            when X"07" =>           -- T SAMPLE II
                CS <= '0';
                spi_out <= '0';
                if (tick_low = '1') then                       
                   spi_next_state <= X"08";
                end if; 
                            
            when X"08" =>           -- NULL BIT
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then                    
                   spi_next_state <= X"09";
                end if;
            
            when X"09" =>           -- B9
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then                     
                   spi_temp(7) <= spi_in;  
                   spi_next_state <= X"0A";
                end if;
            
            when X"0A" =>           -- B8     
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then                      
                   spi_temp(6) <= spi_in; 
                   spi_next_state <= X"0B";
                end if;
            
            when X"0B" =>           -- B7 
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then                    
                   spi_temp(5) <= spi_in;   
                   spi_next_state <= X"0C";
                end if;
                
            when X"0C" =>           -- B6      
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then                    
                   spi_temp(4) <= spi_in;                                
                   spi_next_state <= X"0D";
                end if;
            
            when X"0D" =>           -- B5
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then                     
                   spi_temp(3) <= spi_in; 
                   spi_next_state <= X"0E";
                end if;
                
            when X"0E" =>           -- B4
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then                      
                   spi_temp(2) <= spi_in;
                   spi_next_state <= X"0F";
                end if;
                
            when X"0F" =>           -- B3
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then                      
                   spi_temp(1) <= spi_in; 
                   spi_next_state <= X"10";
                end if;
                
            when X"10" =>           -- B2
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then  
                   spi_temp(0) <= spi_in;  
                   spi_next_state <= X"11";
                end if;
                
            when X"11" =>           -- B1
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then                     
                   spi_next_state <= X"12";
                end if;
                
            when X"12" =>
                CS <= '0';
                spi_out <= '0';
                if (tick_high = '1') then
                   spi_next_state <= X"13";
                end if;
                
            when X"13" =>               -- B0
                CS <= '0';
                spi_out <= '0';
                if (tick_low = '1') then   
                   spi_next_state <= X"14";
                end if;
                
            when X"14" =>               -- TCSH
                CS <= '0';
                spi_out <= '0';
                if (tick_low = '1') then  
                    if (input_select = '0' ) then --and ready = '1'
                      spi_mux_I <=  x"00" & spi_temp;
                      input_select <= '1';
                    elsif (input_select = '1' ) then --and ready = '1'
                      spi_mux_II <=   x"00" & spi_temp;
                      input_select <= '0';
                   end if; 
                   spi_next_state <= X"00";
                end if;
            
            when others =>         
        end case;
    end process;
    
---------------------------------------------------------------------------------------
-- ADDRESS CONTROL LOGIC            ---------------------------------------------------
---------------------------------------------------------------------------------------  

process (address, RE, WE)
begin    
    rx_rd <= '0';                                   -- Resest rx_rd to the default value '0' every clock cycle
    tx_wr <= '0';                                   -- Reset tx_wr to the default value '0' every clock cycle
    p_rx_rd <= '0';                                 -- Resest rx_rd to the default value '0' every clock cycle
    p_tx_wr <= '0';                                 -- Reset tx_wr to the default value '0' every clock cycle
    
    case to_integer(unsigned(address)) is
        when 16#0000# to 16#fdff# => 
            mux_select <= X"0";
            mem_en <= '1';
            
---------- VIRTUAL UART CONNECTION            
        -- STD IN VIRTUAL UART SIGNALS
        when 16#FE00# => mux_select <= X"1";      -- Enables mux output to be the status signal from FIFO STD IN
        when 16#FE02# => 
            if rx_empty = '0' then
                rx_rd <= RE;
                mux_select <= X"2";
            end if;
        -- STD OUT VIRTUAL UART SIGNALS
        when 16#FE04# => mux_select <= X"3";      -- Enables mux output to be the status signal from FIFO STD OUT
        when 16#FE06# =>
            if tx_full = '0' then
                tx_wr <= WE;
            end if;

---------- PHYSICAL UART CONNECTION
        -- STD IN PHYSICAL UART SIGNALS
        when 16#FE20# => mux_select <= X"4";      -- Enables mux output to be the status signal from FIFO STD IN
        when 16#FE22# => 
           if p_rx_empty = '0' then
                p_rx_rd <= RE;
                mux_select <= X"5";
           end if;
        -- STD OUT PHYSICAL UART SIGNALS
        when 16#FE24# => mux_select <= X"6";      -- Enables mux output to be the status signal from FIFO STD OUT
        when 16#FE26# =>
            if p_tx_full = '0' then
                p_tx_wr <= WE;
            end if;                
        
        -- SPI SIGNALS
        when 16#FE34# => mux_select <= X"8";
        when 16#FE35# => mux_select <= X"9";
           
        when others => 
            mem_en <= '1'; 
            mux_select <= X"0";               
    end case;
end process;

---------------------------------------------------------------------------------------  
-- MULTIPLEXER CONTROL SWITCH       ---------------------------------------------------
---------------------------------------------------------------------------------------  

    process (mux_select)
    begin
        case mux_select is
            when X"0" =>  data_in <= ram_out;                             -- Enables read from memory            
        --- VIRTUAL UART   
            when X"1" =>  data_in <= (NOT rx_empty) & "000" & x"000";     -- Reads RX_EMPTY signal from virtual UART
            when X"2" =>  data_in <= x"00" & rx_data;                     -- Reads Data from FIFO.
            when X"3" =>  data_in <= (NOT tx_full) & "000" & x"000";      -- Reads TX_FULL signal from virtual UART            
        --- PHYSICAL UART
            when X"4" =>  data_in <= (NOT p_rx_empty) & "000" & x"000";   -- Reads RX_EMPTY signal from virtual UART
            when X"5" =>  data_in <=  x"00" & p_rx_data;                          -- Reads Data from FIFO.
            when X"6" =>  data_in <= (NOT p_tx_full) & "000" & x"000";    -- Reads TX_FULL signal from virtual UART            
        --- SPI
            when X"8" =>  data_in <= spi_mux_I;
            when X"9" =>  data_in <= spi_mux_II;
            
            when others =>  data_in <= ram_out;                             -- Else, no data is sent through the mux
        end case;
    end process;

    tx_data <= data_out(7 downto 0);
    p_tx_data <= data_out(7 downto 0);
        
---------------------------------------------------------------------------------------  
-- END OF LC3_COMPUTER BEHAVIORAL ARCHITECTURE
---------------------------------------------------------------------------------------  

end Behavioral;

-- *************************************************************************************************************************
-- *************************************************************************************************************************
-- *************************************************************************************************************************
-- *************************************************************************************************************************
-- *************************************************************************************************************************
---------------------------------------------------------------------------------------
-- START OF GRAVEYARD               ---------------------------------------------------
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
-- SWITCH REGISTER, PERSONAL        ---------------------------------------------------
---------------------------------------------------------------------------------------  

    -- sw_reg <= X"00" & sw;       -- Converts the sw_reg 7 downto 0 to 15 downto 0.
   
---------------------------------------------------------------------------------------
-- SEVEN SEGMENT REGISTER, PERSONAL ---------------------------------------------------
---------------------------------------------------------------------------------------  
    --Virtual hexadecimal display on Zybo VIO
    -- hex <= sw_reg;              -- Updates the seven segment display dou to the values found in the register for the switches.

---------------------------------------------------------------------------------------  
-- UNUSED ENTITIES                  ---------------------------------------------------
---------------------------------------------------------------------------------------  

--    lc3_sw_reg: entity work.SW_reg
--    port map(
--        clk =>  clk,
--        sw  =>  sw,
--        sw_reg_data =>  sw_reg
--        );
        
--    lc3_seg7_reg: entity work.seg7_reg
--    port map(
--        clk => clk,
--        rw_en => rw_en,
--        reg_data_in => data_out,
--        sseg => hex
--    );    

---------------------------------------------------------------------------------------
-- FSM TICK                         ---------------------------------------------------
--------------------------------------------------------------------------------------- 
--    type c_state_type is (wait_one, c_low, wait_two, c_high);
--    signal c_state, c_next_state        :   c_state_type;
    
--    process (c_state, tick)
--    begin
----    c_state <= wait_one;
--        case (c_state) is
--            when wait_one =>
--                tick_high <= '0';
--                tick_low  <= '0';
--                if (tick = '1') then
--                    c_next_state <= c_low;
--                else
--                    c_next_state <= wait_one;
--                end if;
--            when c_low =>
--                tick_high <= '0';
--                tick_low  <= '1';
--                c_next_state <= wait_two;
--            when wait_two =>
--                tick_high <= '0';
--                tick_low  <= '0';
--                if (tick = '1') then
--                    c_next_state <= c_high;
--                else
--                    c_next_state <= wait_two;
--                end if;
--            when c_high =>
--                tick_high <= '1';
--                tick_low  <= '0';
--                c_next_state <= wait_one;
--        end case;
--    end process;
   
---------------------------------------------------------------------------------------
-- UNIVERSEL SHIFT REGISTER         ---------------------------------------------------
---------------------------------------------------------------------------------------   
-- SHIFT REGISTER SIGNALS
--    signal shift_ctrl             :   std_logic;
--    signal shift_d                :   std_logic_vector(15 downto 0);
--    signal shift_q                :   std_logic_vector(15 downto 0);
--    signal shift_r_reg            :   std_logic_vector(15 downto 0);
--    signal shift_r_next           :   std_logic_vector(15 downto 0);
          
--   process(clk,reset)
--   begin
--    shift_ctrl <= NOT shift_reg_en;
--      if (reset='1') then
--         shift_r_reg <= (others=>'0');
--      --elsif (load_sr = '1') then 
--      --   shift_r_reg <= spi_status;
--      elsif (clk'event and clk='1') then
--         shift_r_reg <= shift_r_next;
--      end if;
--   end process;
--   -- next-state logic
--   with shift_ctrl select
--    shift_r_next <=
--      shift_r_reg(14 downto 0) & shift_d(0)     when '1',    --shift left;
--      shift_d                                   when others; -- load
--   -- output
--   shift_q <= shift_r_reg;

---------------------------------------------------------------------------------------
-- FSMD STATE REGISTER SPI               ----------------------------------------------
---------------------------------------------------------------------------------------  
--type spi_state_type is (PRE, START, SGL, D_TWO, D_ONE, D_ZERO, SAMPLE_ONE, SAMPLE_TWO, NULLBIT, B_NINE, B_EIGHT, B_SEVEN, B_SIX, B_FIVE, B_FOUR, B_THREE, B_TWO, B_ONE, B_ZERO, B_ZEROO );
--signal spi_state, spi_next_state    :   spi_state_type;

--    signal spi_counter      :   std_logic_vector(7 downto 0);
--    signal sclk             :   std_logic;
--    signal load_SR          :   std_logic;
--    signal shift_reg_en     :   std_logic;
--    signal sample_miso      :   std_logic;
    
--    signal analog_s         :   std_logic_vector(15 downto 0);
--    signal analog_d         :   std_logic_vector(15 downto 0);
    

--process (spi_state, tick_high, tick_low)
--    begin
--       --spi_counter <= x"00";
--       CS          <= '0';
--       sclk        <= '0';
--       load_sr     <= '0';
--       shift_reg_en<= '0'; 
--       sample_MISO <= '1';
--       spi_status_out <= '0';
       
--       spi_status <= spi_status_out;
--       --spi_status <= tick_high;
--       --spi_cs <= tick_low;
--       spi_cs <= NOT CS;
               
--        case (spi_state) is
        
--            when SPI_INIT =>
--                spi_counter <= x"00";
--                CS          <= '0';
--                sclk        <= '0';
--                load_sr     <= '0';
--                shift_reg_en<= '0';
                
--                if (address = x"FE34") then         -- and WE = '1'
--                    spi_next_state <= SPI_START;
--                else
--                    spi_next_state <= SPI_INIT;
--                end if;
                
--            when SPI_START =>   
--                spi_counter <= x"00";
                
--                if (tick_low = '1') then                            
--                    spi_counter <= spi_counter;
--                    CS          <= '1';
--                    sclk        <= '0';
--                    load_sr     <= '1';
--                    shift_reg_en<= '1';
--                    spi_next_state <= SPI_START_BIT;
--                end if;
                
--            when SPI_START_BIT =>
--                spi_counter <= spi_counter;
--                CS          <= '1';
--                sclk        <= '0';
--                load_sr     <= '1';
--                shift_reg_en<= '1';
--                spi_next_state <= SPI_START_BIT;
                
--                if tick_low = '1' then
--                 -- shift_reg_en <= '1';
--                    spi_status_out <= '1';
--                    spi_next_state <= SPI_SGL_BIT;
--                end if;
                
--            when SPI_SGL_BIT =>
                
--                -- shift_reg_en <= '1';
--                spi_status_out <= '1';                
--                spi_next_state <= SPI_SGL_BIT;  
--                if tick_low = '1' then
--                 -- shift_reg_en <= '1';
--                    spi_status_out <= '1';
--                    spi_next_state <= SPI_D_TWO;
--                end if; 
           
--            when SPI_D_TWO =>
--                -- shift_reg_en <= '1';
--                spi_status_out <= '1';                                                
--                spi_next_state <= SPI_D_TWO; 
--                if tick_low = '1' then
--                 -- shift_reg_en <= '1';
--                    spi_status_out <= '0';
--                    spi_next_state <= SPI_D_ONE;
--                end if; 
                
--            when SPI_D_ONE =>
--                -- shift_reg_en <= '1';
--                spi_status_out <= '0';                                                                  
--                spi_next_state <= SPI_D_ONE; 
                
--                if tick_low = '1' then
--                 -- shift_reg_en <= '1';
--                    spi_status_out <= '0';
--                    spi_next_state <= SPI_D_ZERO;
--                end if; 
                
--            when SPI_D_ZERO =>
--                -- shift_reg_en <= '1';
--                spi_status_out <= '0';                                                                  
--                spi_next_state <= SPI_D_ZERO; 
--                if tick_low = '1' then
--                 -- shift_reg_en <= '1';
--                    spi_status_out <= '0';
--                    spi_next_state <= SPI_WAIT;
--                end if; 
                
--            when SPI_WAIT =>
--                spi_counter <= spi_counter;
--                CS          <= '1';
--                sclk        <= '0';
--                load_sr     <= '0';
--                shift_reg_en<= '0';
                
--                if (tick_high = '1') then
--                    spi_next_state <= SPI_HIGH;
--                elsif (tick_low = '1') then
--                    spi_next_state <= SPI_LOW;
--                else
--                    spi_next_state <= SPI_WAIT;
--                end if;
            
--            when SPI_HIGH =>
--                spi_counter <= spi_counter + '1';
--                cs          <= '1';
--                sclk        <= '1';         -- LOAD
--                sample_miso <= '1';
--                spi_next_state <= SPI_WAIT;
                
--            when SPI_LOW  =>
--                spi_counter <= spi_counter;
--                CS          <= '1';
--                sclk        <= '0';         -- CLEAR
--                load_sr     <= '0';
--                shift_reg_en<= '1';
                
--                if (spi_counter = x"10" OR spi_counter > x"10") then
--                    spi_next_state <= SPI_INIT;
--                else
--                    spi_next_state <= SPI_WAIT;
--                end if;
--        end case;
--    end process;