LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY tb_uart_i2c_interconnect IS
END tb_uart_i2c_interconnect;

ARCHITECTURE behavior OF tb_uart_i2c_interconnect IS
  SIGNAL clk : STD_LOGIC := '0';
  SIGNAL reset_n : STD_LOGIC := '0';
  -- Define other input signals here
  SIGNAL rx : STD_LOGIC := '0';
  SIGNAL tx : STD_LOGIC;
 constant TbPeriod : time := 20 ns; -- EDIT Put right period here
    signal TbClock : std_logic := '0';  
  SIGNAL sda : std_logic_vector(1 downto 0) ;
  SIGNAL scl : std_logic_vector(1 downto 0) ;
  SIGNAL uart_tx_data : std_logic_vector(7 downto 0)  :="00000000";  --data that uart transmitt
  constant LETTER_C : std_logic_vector(7 downto 0)  := "01000011";
  constant LETTER_O : std_logic_vector(7 downto 0)  := "01001111";
  constant LETTER_N : std_logic_vector(7 downto 0)  := "01001110";
  constant LETTER_I : std_logic_vector(7 downto 0)  := "01001001";
  constant LETTER_2 : std_logic_vector(7 downto 0)  := "00110010";
  constant i2c_slave_id0_write : std_logic_vector(7 downto 0)  := "00000000"; --adress and write command for slave 1
  constant i2c_slave_id0_read : std_logic_vector(7 downto 0)  := "10000000";  --adress and read command for slave 1
  constant i2c_slave_id1_write : std_logic_vector(7 downto 0)  := "00000001"; --adress and write command for slave 2
  constant i2c_slave_id1_read : std_logic_vector(7 downto 0)  := "10000001"; --adress and read command for slave 2
  constant change_config  : std_logic_vector(7 downto 0)  := "11001110";  --no parity, 115200 br, 2 stop bits, 1 Mhz I2C speed
  constant random_data_0  : std_logic_vector(7 downto 0)  := "11010101";
  constant random_data_1  : std_logic_vector(7 downto 0)  := "01010001";
  -- Define your instance of i2c_master module here
  COMPONENT uart_i2c_interconnect
   GENERIC(
      clk_freq  :  INTEGER    := 50000000;  --frequency of system clock in Hertz
      uart_os_rate   :  INTEGER    := 16;          --oversampling rate to find center of receive bits (in samples per baud period)
      uart_d_width   :  INTEGER    := 8;
      no_of_slaves    : integer := 2);
   PORT(
      clock   : IN    STD_LOGIC;  --system clock
      reset_n : IN    STD_LOGIC;  --active low reset
      rx    : IN    STD_LOGIC;  --uart data in
      tx    : OUT   STD_LOGIC;  --uart data out
      scl     : INOUT std_logic_vector(no_of_slaves-1 downto 0);  --i2c serial clock
      sda     : INOUT std_logic_vector(no_of_slaves-1 downto 0));  --i2c serial data 
  END COMPONENT;

BEGIN
  -- Instantiate your i2c_master module here
  -- i2c_inst : i2c_master PORT MAP(...);
   uut: uart_i2c_interconnect
    GENERIC MAP (
      clk_freq     =>  50000000,  --frequency of system clock in Hertz
      uart_os_rate    =>  16,          --oversampling rate to find center of receive bits (in samples per baud period)
      uart_d_width   =>  8,
      no_of_slaves    =>  2
    )
    PORT MAP (
      tx => tx,
      clock => clk,
      reset_n => reset_n,
      rx => rx,
      sda => sda,
      scl => scl
    );
  -- Clock generation process
   TbClock <= not TbClock after TbPeriod/2;
    
    -- EDIT: Check that clk is really your main clock signal
    clk <= TbClock;

  -- Stimulus process
  PROCESS


  procedure I2C_Slave_Read(Data: std_logic_vector; time :time; slave_device : integer) is  --slave respond to read request
    begin
      for i in 7 downto 0 loop
         sda(slave_device) <= Data(i);
         wait for time;
      end loop;
    end procedure I2C_Slave_Read;
    
  procedure Write(Data: std_logic_vector; period :integer) is --write command to uart
    begin
      for i in 0 to 7 loop
         rx  <= Data(i);
         wait for period*TbPeriod;
      end loop;
    end procedure Write;
    
  procedure setup_uart(period :integer) is --before command setup is needed
    begin
       rx <= '1';
       wait for period*TbPeriod;
       rx  <= '0';
       wait for period*TbPeriod;
    end procedure setup_uart;
    
   variable i : integer ;
  BEGIN
    sda(0) <= 'Z';
    sda(1) <= 'Z';
    reset_n <= '0';        --assert reset
    WAIT FOR TbPeriod;
    reset_n <= '1';
    WAIT FOR TbPeriod;

   Write(change_config,5208);       --sending new configuration but won't change it untill "CON" sequence is sent
   setup_uart(5208);
   Write(LETTER_C,5208);
   setup_uart(5208);
   Write(LETTER_O,5208);   
   setup_uart(5208);
   Write(LETTER_N,5208);
   setup_uart(5208);
   Write(change_config,5208);      --configuration is changed
  
   wait for 5208*TbPeriod;
   
   setup_uart(434);
   Write(LETTER_I,434);
   setup_uart(434);
   Write(LETTER_2,434);   
   setup_uart(434);
   Write(LETTER_C,434);             --I2C sequence is read
   setup_uart(434);
   Write(i2c_slave_id0_read,434);   --slave adress and w/r command
   setup_uart(434);
   Write(random_data_0,434);        --data to write if needed
    
    rx  <= '1';                     --hardcoded waiting time for sending NACK 
    wait for 434*TbPeriod;
    rx  <= '0';
    wait for TbPeriod;
    rx  <= '1';
    wait for 5 us - TbPeriod;
    sda(0) <= '0';                  --slave send NACK
    wait for 1500 ns;

   I2C_Slave_Read(random_data_0,950 ns,0); --i2c send data for i2c master to read
   
    sda(0) <= 'Z';                  --release SDA
    
    wait for 25 us;
    

     uart_tx_data(7) <= tx;         --read tx output as fast as baud rate
     wait for 9 us;
     uart_tx_data(6) <= tx;
     wait for 9 us;
     uart_tx_data(5) <= tx;
     wait for 9 us;
     uart_tx_data(4) <= tx;
     wait for 9 us;
     uart_tx_data(3) <= tx;
     wait for 9 us;
     uart_tx_data(2) <= tx;
     wait for 9 us;
     uart_tx_data(1) <= tx;
     wait for 9 us;
     uart_tx_data(0) <= tx;
     wait for 10 us;

    IF uart_tx_data = random_data_0 THEN
      REPORT "Output data correspond to expected read value" SEVERITY NOTE;
    ELSE
      REPORT "Output data does not correspond to expected read value" SEVERITY ERROR;
    END IF;

      setup_uart(434);
      Write(LETTER_I,434);
      setup_uart(434);
      Write(LETTER_2,434);   
      setup_uart(434);
      Write(LETTER_C,434);
      setup_uart(434);
      Write(i2c_slave_id0_write,434);     --command to write
      setup_uart(434);
      Write(random_data_1,434);           --data to write
       rx  <= '1';         
                     
                     
                     
                                          --no NACK to write from slave to command is rejected
      wait;
    END PROCESS;

END behavior;