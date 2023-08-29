LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY tb_uart IS
END tb_uart;

ARCHITECTURE testbench OF tb_uart IS
  SIGNAL clk, reset_n, tx_ena, rx, tx, rx_busy, rx_error, tx_busy, stop_bits : STD_LOGIC := '0';
  SIGNAL parity : std_logic_vector(1 downto 0) := "01";  -- Set your desired test parameters signal
  signal baud_rate : std_logic_vector(2 downto 0); 
 
 constant TbPeriod : time := 542534 ps; -- EDIT Put right period here
    signal TbClock : std_logic := '0';  
  signal tx_data, rx_data : std_logic_vector(7 downto 0);
  signal tx_data_test : std_logic_vector(7 downto 0);
  COMPONENT uart
GENERIC(
    clk_freq  :  INTEGER    := 1843200;  --frequency of system clock in Hertz
    os_rate   :  INTEGER    := 16;          --oversampling rate to find center of receive bits (in samples per baud period)
    d_width   :  INTEGER    := 8);           --data bus width
  PORT(
    stop_bits      :  IN   STD_LOGIC;
    parity_bits : IN   STD_LOGIC_VECTOR(1 DOWNTO 0);
    baud_rate : IN   STD_LOGIC_VECTOR(2 DOWNTO 0);
    clk      :  IN   STD_LOGIC;                             --system clock
    reset_n  :  IN   STD_LOGIC;                             --ascynchronous reset
    tx_ena   :  IN   STD_LOGIC;                             --initiate transmission
    tx_data  :  IN   STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --data to transmit
    rx       :  IN   STD_LOGIC;                             --receive pin
    rx_busy  :  OUT  STD_LOGIC;                             --data reception in progress
    rx_error :  OUT  STD_LOGIC;                             --start, parity, or stop bit error detected
    rx_data  :  OUT  STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --data received
    tx_busy  :  OUT  STD_LOGIC;                             --transmission in progress
    tx       :  OUT  STD_LOGIC);                            --transmit pin
  END COMPONENT;

BEGIN

  -- Instantiate the uart module
  uut: uart
    GENERIC MAP (
      clk_freq => 1843200,
      os_rate => 16,
      d_width => 8
    )
    PORT MAP (
      stop_bits => stop_bits,  
      parity_bits => parity,
      baud_rate => baud_rate,
      clk => clk,
      reset_n => reset_n,
      tx_ena => tx_ena,
      tx_data => tx_data,
      rx => rx,
      rx_busy => rx_busy,
      rx_error => rx_error,
      rx_data => rx_data,
      tx_busy => tx_busy,
      tx => tx
    );
   TbClock <= not TbClock after TbPeriod/2;

    -- EDIT: Check that clk is really your main clock signal
    clk <= TbClock;
  -- Clock generation

  -- Stimulus process
  PROCESS
  BEGIN
    reset_n <= '0';
    WAIT FOR TbPeriod;
    reset_n <= '1';
    WAIT FOR TbPeriod;
    rx <= '1';
    baud_rate <=  "111"; --115200 baud rate
    parity  <=  "00"; --no parity bit
    stop_bits <=  '1'; --2 stop bits
    
    -- Simulate sending data
    tx_ena <= '1';
    tx_data <= x"57";  
    WAIT for TbPeriod;
    tx_ena <= '0';
     wait for 40*TbPeriod;
    
    tx_data_test(0) <= tx;         --read tx output as fast as baud rate
     wait for 16*TbPeriod;
     tx_data_test(1) <= tx;
     wait for 16*TbPeriod;
     tx_data_test(2) <= tx;
     wait for 16*TbPeriod;
     tx_data_test(3) <= tx;
     wait for 16*TbPeriod;
     tx_data_test(4) <= tx;
     wait for 16*TbPeriod;
     tx_data_test(5) <= tx;
     wait for 16*TbPeriod;
     tx_data_test(6) <= tx;
     wait for 16*TbPeriod;
     tx_data_test(7) <= tx;
     wait until tx_busy = '0';
     IF tx_data_test = x"57" THEN
      REPORT "Passed! Output data is as expected" SEVERITY NOTE;
    ELSE
      REPORT "Wrong data on output" SEVERITY ERROR;
    END IF;
      

     
    WAIT for 500*TbPeriod;

    WAIT for TbPeriod;        --start bit
    rx  <= '0';
    
    wait for 16*TbPeriod;     --start sending data
    rx  <= '1';
    wait for 16*TbPeriod;
    rx  <= '0';
    wait for 16*TbPeriod;
    rx  <= '1';
    wait for 16*TbPeriod;
    rx  <= '1';
    wait for 16*TbPeriod;
    rx  <= '1';
    wait for 16*TbPeriod;
    rx  <= '0';
     wait for 16*TbPeriod;
    rx  <= '1';
    wait for 16*TbPeriod;
    rx  <= '0';
--    wait for 16*TbPeriod;     
--    rx  <= '0';            --parity bit
    wait for 16*TbPeriod;
    rx  <= '0';            --stop bit
    wait for 16*TbPeriod;
    rx  <= '1';            --stop bit
    wait for 16*TbPeriod;
    IF rx_error = '1' THEN
      REPORT "Passed! 1st stop bit is '0' so rx_error must be asserted" SEVERITY NOTE;
    ELSE
      REPORT "Rx_error must be asserted because 1st top bit is '0'" SEVERITY ERROR;
    END IF;
    IF rx_data = x"57" THEN
      REPORT "Passed! Expected value is read" SEVERITY NOTE;
    ELSE
      REPORT "Wrong value is read" SEVERITY ERROR;
    END IF;
    
    baud_rate <=  "011"; --57600 baud rate
    parity  <=  "10"; --even parity 
    stop_bits <=  '0'; --1 stop bit
    WAIT for TbPeriod;        --start bit
    rx  <= '0';
    
    wait for 32*TbPeriod;     --start sending data
    rx  <= '1';
    wait for 32*TbPeriod;
    rx  <= '0';
    wait for 32*TbPeriod;
    rx  <= '1';
    wait for 32*TbPeriod;
    rx  <= '1';
    wait for 32*TbPeriod;
    rx  <= '1';
    wait for 32*TbPeriod;
    rx  <= '0';
     wait for 32*TbPeriod;
    rx  <= '1';
    wait for 32*TbPeriod;
    rx  <= '0';
    wait for 32*TbPeriod;     
    rx  <= '0';            --parity bit
--    wait for 32*TbPeriod;
--    rx  <= '0';            --stop bit
    wait for 32*TbPeriod;
    rx  <= '1';            --stop bit
    wait for 32*TbPeriod;
    IF rx_error = '1' THEN
      REPORT "Rx_error must not be asserted because parity bit should be '0' when all bits XOR equal '1'" SEVERITY ERROR;
    ELSE
      REPORT "Passed!" SEVERITY NOTE;
    END IF;
    
    wait;
    END PROCESS;

END testbench;