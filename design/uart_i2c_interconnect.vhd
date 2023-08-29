LIBRARY ieee;
USE ieee.std_logic_1164.all;
package i2c_arrays_pkg is
   type i2c_arrays is array(natural range <>) of std_logic_vector(7 downto 0);
   type i2c_addr_array is array(natural range <>) of std_logic_vector(6 downto 0);
end package;

LIBRARY ieee;
USE ieee.std_logic_1164.all;
use work.i2c_arrays_pkg.all;

ENTITY uart_i2c_interconnect IS
   GENERIC(
      clk_freq  :  INTEGER    := 50000000;  --frequency of system clock in Hertz
      uart_os_rate   :  INTEGER    := 16;   --oversampling rate to find center of receive bits (in samples per baud period)
      uart_d_width   :  INTEGER    := 8;    --uart data width
      no_of_slaves    : integer := 2);      --number of i2c slaves
   PORT(
      clock   : IN    STD_LOGIC;  --system clock
      reset_n : IN    STD_LOGIC;  --active low reset
      rx    : IN    STD_LOGIC;  --uart data in
      tx    : OUT   STD_LOGIC;  --uart data out
      scl     : INOUT std_logic_vector(no_of_slaves-1 downto 0);  --i2c serial clock
      sda     : INOUT std_logic_vector(no_of_slaves-1 downto 0));  --i2c serial data
END uart_i2c_interconnect;

ARCHITECTURE logic OF uart_i2c_interconnect IS



   SIGNAL   uart_tx_ena    : STD_LOGIC;
   SIGNAL   uart_rx_error  : STD_LOGIC;
   SIGNAL   uart_tx_data : STD_LOGIC_VECTOR(7 DOWNTO 0);
   SIGNAL   uart_rx_data : STD_LOGIC_VECTOR(7 DOWNTO 0);
   SIGNAL   uart_rx_busy    : STD_LOGIC;
   SIGNAL   uart_tx_busy    : STD_LOGIC;
   SIGNAL   i2c_ena     : STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);
   SIGNAL   i2c_addr    : i2c_addr_array(no_of_slaves-1 DOWNTO 0);
   SIGNAL   i2c_rw      : STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);
   SIGNAL   i2c_data_wr : i2c_arrays(no_of_slaves-1 DOWNTO 0);
   SIGNAL   i2c_data_rd : i2c_arrays(no_of_slaves-1 DOWNTO 0);
   SIGNAL   i2c_ack_err : STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);
   SIGNAL   i2c_busy    : STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);
   SIGNAL   i2c_accept    : STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);
   SIGNAL   i2c_finished    : STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);
   SIGNAL   uart_baud_rate : STD_LOGIC_VECTOR(2 DOWNTO 0);
   SIGNAL   i2c_bus_clk : STD_LOGIC_VECTOR(1 DOWNTO 0);
   SIGNAL   uart_parity_bits : STD_LOGIC_VECTOR(1 DOWNTO 0);
   SIGNAL   uart_stop_bits : STD_LOGIC;

   COMPONENT uart_i2c IS
      PORT(
         i2c_bus_clk   : out     STD_LOGIC_VECTOR(1 DOWNTO 0);                   --speed of i2c slaves
         uart_stop_bits       : out   STD_LOGIC;                                 --stop bits(1 or 2)
         uart_parity_bits     : out   STD_LOGIC_VECTOR(1 DOWNTO 0);              --parity (0 or 1)
         uart_baud_rate       : out   STD_LOGIC_VECTOR(2 DOWNTO 0);              --baud rate
         clk             : IN   STD_LOGIC;                                       --system clock
         reset_n         : IN   STD_LOGIC;                                       --active low reset
         uart_tx_ena     : out STD_LOGIC;                                        --initiate transmission
         uart_tx_busy    : in std_logic;                                         --transmission in progress
         uart_rx_error   : in STD_LOGIC;                                         --start, parity, or stop bit error detected
         uart_tx_data    : out STD_LOGIC_VECTOR(7 DOWNTO 0);                     --data to transmit
         uart_rx_data    : in STD_LOGIC_VECTOR(7 DOWNTO 0);                      --data received
         uart_rx_busy    : in STD_LOGIC;                                         --data reception in progress
         i2c_finished     : IN   STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);      --i2c finished read command
         i2c_accept      : IN   STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);       --i2c accepted command w/r
         i2c_busy        : IN   STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);       --i2c busy signal (talking to i2c slave)
         i2c_data_rd     : IN   i2c_arrays(no_of_slaves-1 DOWNTO 0);             --data received from i2c slave
         i2c_ack_err     : IN   STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);       --i2c acknowledge error flag
         i2c_ena         : OUT  STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);       --latch command into i2c master
         i2c_addr        : OUT  i2c_addr_array(no_of_slaves-1 DOWNTO 0);         --i2c slave address
         i2c_rw          : OUT  STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);       --i2c read/write command
         i2c_data_wr     : OUT  i2c_arrays(no_of_slaves-1 DOWNTO 0));            --data to write over the i2c bus
   END COMPONENT uart_i2c;
   COMPONENT uart IS
      GENERIC(
         clk_freq  :  INTEGER    := 1843200;                      --frequency of system clock in Hertz
         os_rate   :  INTEGER    := 16;                           --oversampling rate to find center of receive bits (in samples per baud period)
         d_width   :  INTEGER    := 8);                           --data bus width

      PORT(
         stop_bits      :  IN   STD_LOGIC;                       --stop bits(1 or 2)
         parity_bits : IN   STD_LOGIC_VECTOR(1 DOWNTO 0);        --parity (0 or 1)
         baud_rate : IN   STD_LOGIC_VECTOR(2 DOWNTO 0);          --baud rate
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
   END COMPONENT uart;

   --declare i2c master component
   COMPONENT i2c IS
      GENERIC(
         input_clk : INTEGER);  --input clock speed from user logic in Hz
      PORT(
         bus_clk   : in     STD_LOGIC_VECTOR(1 DOWNTO 0); --speed of i2c slave
         clk       : IN     STD_LOGIC;                    --system clock
         reset_n   : IN     STD_LOGIC;                    --active low reset
         ena       : IN     STD_LOGIC;                    --latch in command
         addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
         rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
         data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
         finish      : OUT    STD_LOGIC;                    --indicates transaction in progress
         busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
         accept      : OUT    STD_LOGIC;                    --indicates transaction in progress
         data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
         ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
         sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
         scl       : INOUT  STD_LOGIC);                   --serial clock output of i2c bus
   END COMPONENT i2c;

BEGIN

   uart_i2c_0:  uart_i2c
      PORT MAP(i2c_finished=> i2c_finished, i2c_bus_clk => i2c_bus_clk,uart_stop_bits  =>uart_stop_bits,uart_baud_rate => uart_baud_rate,
               uart_parity_bits => uart_parity_bits, clk => clock, reset_n => reset_n,
               uart_rx_error => uart_rx_error,
               uart_rx_data => uart_rx_data, uart_rx_busy => uart_rx_busy,
               uart_tx_ena => uart_tx_ena,
               uart_tx_data => uart_tx_data, uart_tx_busy => uart_tx_busy,i2c_accept => i2c_accept,i2c_ena => i2c_ena, i2c_addr => i2c_addr,
               i2c_rw => i2c_rw, i2c_data_wr => i2c_data_wr, i2c_busy => i2c_busy,
               i2c_data_rd => i2c_data_rd, i2c_ack_err => i2c_ack_err);
   --instantiate the bridge component
   uart_0:  uart
      GENERIC MAP(clk_freq => clk_freq, os_rate => uart_os_rate, d_width => uart_d_width)
      PORT MAP(stop_bits  =>uart_stop_bits,baud_rate => uart_baud_rate, parity_bits => uart_parity_bits, tx =>tx, rx  => rx,clk => clock, reset_n => reset_n, rx_error => uart_rx_error,
               rx_data => uart_rx_data, rx_busy => uart_rx_busy,
               tx_ena => uart_tx_ena,
               tx_data => uart_tx_data, tx_busy => uart_tx_busy);

   i2c_0 : FOR n IN (no_of_slaves-1) DOWNTO 0 GENERATE
      i2c_gen: i2c
         GENERIC MAP(input_clk => clk_freq)
         PORT MAP(finish => i2c_finished(n), bus_clk => i2c_bus_clk,clk => clock, reset_n => reset_n, ena => i2c_ena(n), addr => i2c_addr(n),
                  rw => i2c_rw(n), data_wr => i2c_data_wr(n), busy => i2c_busy(n), accept  => i2c_accept(n),
                  data_rd => i2c_data_rd(n), ack_error => i2c_ack_err(n), sda => sda(n),
                  scl => scl(n));
   END GENERATE i2c_0;


END logic;
