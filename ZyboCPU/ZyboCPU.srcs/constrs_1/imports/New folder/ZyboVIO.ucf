## This file is a general .ucf for the ZYBO Rev B board
## To use it in a project:
## - uncomment the lines corresponding to used pins
## - rename the used signals according to the project
##########################################################################################

## Clock signal
NET "clk125"        LOC=L16 | IOSTANDARD=LVCMOS33; #IO_L11P_T1_SRCC_35	
NET "clk125" TNM_NET = sys_clk_pin;
TIMESPEC TS_sys_clk_pin = PERIOD sys_clk_pin 125 MHz HIGH 50%; 
##########################################################################################
#
NET * IOSTANDARD=LVCMOS33; 
## Switches
NET "psw<0>"         LOC=G15; #psw<0>
NET "psw<1>"         LOC=P15; #psw<1>
NET "psw<2>"         LOC=W13; #psw<2>
NET "psw<3>"         LOC=T16; #psw<3>
###########################################################################################
#
## Buttons
NET "pbtn<0>"        LOC=R18; #pbtn<0>
NET "pbtn<1>"        LOC=P16; #pbtn<1>
NET "pbtn<2>"        LOC=V16; #pbtn<2>
NET "pbtn<3>"        LOC=Y16; #pbtn<3>
###########################################################################################
#
## LEDs
NET "pled<0>"        LOC=M14; #pled<0>
NET "pled<1>"        LOC=M15; #pled<1>
NET "pled<2>"        LOC=G14; #pled<2>
#NET "pled<3>"        LOC=D18; #pled<3>
NET "blinky"         LOC=D18; #pled<3>
##########################################################################################
#
## Pmod Header JE
#NET "je<0>"         LOC=V12; #je<0>
#NET "je<1>"         LOC=W16; #je<1>
#NET "je<2>"         LOC=J15; #je<2>
#NET "je<3>"         LOC=H15; #je<3>
#NET "je<4>"         LOC=V13; #je<4>
#NET "je<5>"         LOC=U17; #je<5>
#NET "je<6>"         LOC=T17; #je<6>
#NET "je<7>"         LOC=Y17; #je<7>
##########################################################################################

