## This file is a simplified .xdc for the ZYBO board
## To use it in a project:
## - uncomment the lines corresponding to used pins
## - rename the used signals according to the project

set_property IOSTANDARD LVCMOS33 [get_ports *]

##Clock signal
set_property PACKAGE_PIN L16 [get_ports clk125]
create_clock -add -name sys_clk_pin -period 8.00 -waveform {0 4} [get_ports clk125]

##Switches
set_property PACKAGE_PIN G15 [get_ports {psw[0]}]
set_property PACKAGE_PIN P15 [get_ports {psw[1]}]
set_property PACKAGE_PIN W13 [get_ports {psw[2]}]
set_property PACKAGE_PIN T16 [get_ports {psw[3]}]

##Buttons
set_property PACKAGE_PIN R18 [get_ports {pbtn[0]}]
set_property PACKAGE_PIN P16 [get_ports {pbtn[1]}]
set_property PACKAGE_PIN V16 [get_ports {pbtn[2]}]
set_property PACKAGE_PIN Y16 [get_ports {pbtn[3]}]

##LEDs
set_property PACKAGE_PIN M14 [get_ports {pled[0]}]
set_property PACKAGE_PIN M15 [get_ports {pled[1]}]
set_property PACKAGE_PIN G14 [get_ports {pled[2]}]
set_property PACKAGE_PIN D18 [get_ports {blinky}]


##Pmod Header JE
#set_property PACKAGE_PIN V12 [get_ports {je[0]}] 
#set_property PACKAGE_PIN W16 [get_ports {je[1]}] 
#set_property PACKAGE_PIN J15 [get_ports {je[2]}] 
#set_property PACKAGE_PIN H15 [get_ports {je[3]}] 
#set_property PACKAGE_PIN V13 [get_ports {je[4]}] 
#set_property PACKAGE_PIN U17 [get_ports {je[5]}] 
#set_property PACKAGE_PIN T17 [get_ports {je[6]}] 
#set_property PACKAGE_PIN Y17 [get_ports {je[7]}] 

##VGA Connector
#set_property PACKAGE_PIN M19 [get_ports {vga_r[0]}] 
#set_property PACKAGE_PIN L20 [get_ports {vga_r[1]}] 
#set_property PACKAGE_PIN J20 [get_ports {vga_r[2]}] 
#set_property PACKAGE_PIN G20 [get_ports {vga_r[3]}] 
#set_property PACKAGE_PIN F19 [get_ports {vga_r[4]}] 
#set_property PACKAGE_PIN H18 [get_ports {vga_g[0]}] 
#set_property PACKAGE_PIN N20 [get_ports {vga_g[1]}] 
#set_property PACKAGE_PIN L19 [get_ports {vga_g[2]}] 
#set_property PACKAGE_PIN J19 [get_ports {vga_g[3]}] 
#set_property PACKAGE_PIN H20 [get_ports {vga_g[4]}] 
#set_property PACKAGE_PIN F20 [get_ports {vga_g[5]}] 
#set_property PACKAGE_PIN P20 [get_ports {vga_b[0]}] 
#set_property PACKAGE_PIN M20 [get_ports {vga_b[1]}] 
#set_property PACKAGE_PIN K19 [get_ports {vga_b[2]}] 
#set_property PACKAGE_PIN J18 [get_ports {vga_b[3]}] 
#set_property PACKAGE_PIN G19 [get_ports {vga_b[4]}] 
#set_property PACKAGE_PIN P19 [get_ports vga_hs]     
#set_property PACKAGE_PIN R19 [get_ports vga_vs]     