# UART-I2C-Interconnect
UART-I2C Interconnect for Eridan Pre - screening Test 2.0

UART-I2C Interconnect is interface between UART master and variable number of I2C slave devices (set on instantiation).
For now, it has 2 types of commands, changing configuration and sending I2C requests. 
For changing configuration user need to send "CON" array of characters and for i2c requests "I2C" array of characters. 

If "CON" then next byte from UART needs to contain new conifguration by these rules : 
1st bit correspond to change number of stop bits. ('0' for 1 bit and '1' for 2 bits).
2nd and 3rd to change baud rate ( "000" =>baud <= 9600; "001" => baud <= 19200; "010" => baud <= 38400; "011" =>  baud <= 57600; "111" => baud <= 115200).
4th and 5th to change number and sort of parity ("00" => no parity; "01"=> odd parity; "10"=> even parity).
last 2 bits to change i2c slaves speed ("00" => i2c_clk <= 100000; "01" => i2c_clk <= 400000; "10" => i2c_clk <= 500000; "11" =>i2c_clk <= 1000000;).

If "I2C" then next byte from UART needs to contain adress (6 downto 0) and w/r (7).
After that next byte is data to be written on I2C slave (if W).
