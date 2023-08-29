LIBRARY ieee;
USE ieee.std_logic_1164.all;
use work.i2c_arrays_pkg.all;
use ieee.numeric_std.all;

ENTITY uart_i2c IS
   generic (
      no_of_slaves    : integer := 2);
   PORT(
      i2c_bus_clk   : out     STD_LOGIC_VECTOR(1 DOWNTO 0);
      uart_stop_bits       : out   STD_LOGIC;
      uart_parity_bits     : out   STD_LOGIC_VECTOR(1 DOWNTO 0);
      uart_baud_rate       : out   STD_LOGIC_VECTOR(2 DOWNTO 0);
      clk             : IN   STD_LOGIC;                    
      reset_n         : IN   STD_LOGIC;                     
      uart_tx_ena     : out STD_LOGIC;
      uart_rx_error   : in STD_LOGIC;
      uart_tx_data    : out STD_LOGIC_VECTOR(7 DOWNTO 0);
      uart_tx_busy    : in STD_LOGIC;
      uart_rx_data    : in STD_LOGIC_VECTOR(7 DOWNTO 0);
      uart_rx_busy    : in STD_LOGIC;
      i2c_finished     : IN   STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);
      i2c_accept      : IN   STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);
      i2c_busy        : IN   STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);              
      i2c_data_rd     : IN   i2c_arrays(no_of_slaves-1 DOWNTO 0); 
      i2c_ack_err     : IN   STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);                    
      i2c_ena         : OUT  STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);                    
      i2c_addr        : OUT  i2c_addr_array(no_of_slaves-1 DOWNTO 0);  
      i2c_rw          : OUT  STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0);                    
      i2c_data_wr     : OUT  i2c_arrays(no_of_slaves-1 DOWNTO 0)); 
END uart_i2c;

ARCHITECTURE behavior OF uart_i2c IS
   signal current : integer :=-1;                        --which i2c is currently sendind on uart
   TYPE machine IS(ready, config, i2c, i2c_command);     --state machine datatype
   TYPE uart_array  is array(natural range <>) of std_logic_vector(7 downto 0);
   signal uart_buffer : uart_array(2 downto 0) := (others=>(others=> '0')); --array for input data 
   SIGNAL state         : machine;                       --current state
   SIGNAL uart_rx_busy_prev : STD_LOGIC;                     --previous busy signal for i2c transactions
   SIGNAL uart_rx_data_prev_addr : STD_LOGIC_VECTOR(6 DOWNTO 0);                  --previous address for i2c transactions
   SIGNAL sent_flags : STD_LOGIC_VECTOR(no_of_slaves-1 DOWNTO 0):= (others=> '0');      --waiting line for slaves, if 1 then waiting to send
begin

   process(Clk,reset_n) is
   begin
      IF(reset_n = '0') then
         uart_rx_busy_prev <= '0';
         uart_rx_data_prev_addr <= (others=>'0');
      elsif rising_edge(Clk) then
         uart_rx_busy_prev <= uart_rx_busy ;
         uart_rx_data_prev_addr <=  uart_rx_data(6 downto 0);
      end if;
   end process;

   PROCESS(clk, reset_n)
      VARIABLE uart_buffer_count : INTEGER := 0;
   BEGIN
      IF(reset_n = '0') THEN                 
         current <= -1;
         i2c_bus_clk <= "00";
         i2c_ena <= (others=>'0');
         uart_tx_ena <= '0';
         uart_tx_data <=  (others=>'0');
         state <= ready;
         i2c_rw <=  (others=>'0');
         i2c_data_wr  <= (others=>(others=> '0'));
         i2c_addr  <= (others=>(others=> '0'));
         uart_stop_bits  <= '0';
         uart_baud_rate  <= "000";
         uart_parity_bits  <= "00";
         uart_buffer_count  := 0;
         uart_buffer <= (others=>(others=> '0'));
         sent_flags <=  (others=>'0');
      ELSIF(rising_edge(Clk)) THEN
         CASE state IS
            WHEN ready =>
               if (uart_buffer_count = 3) then           --commands contains 3 letters always 
                  uart_buffer_count  := 0;               --if received is not "I2C" or "CON" restart counter and buffer
                  uart_buffer <= (others=>(others=> '0'));
               end if;

               IF(uart_rx_busy = '0' AND uart_rx_busy_prev = '1' and uart_rx_error = '0') THEN      --new byte from uart 
                  if(uart_buffer_count = 2) then
                     if (uart_buffer(uart_buffer_count-2) = x"43" and uart_buffer(uart_buffer_count-1) = x"4F" and uart_rx_data= x"4E") then --are last 3 bytes "CON"
                        state <= config;
                        uart_buffer <= (others=>(others=> '0'));
                     elsif (uart_buffer(uart_buffer_count-2) = x"49" and uart_buffer(uart_buffer_count-1) = x"32" and uart_rx_data= x"43") then --are last 3 bytes "I2C"
                        state <= i2c;
                        uart_buffer <= (others=>(others=> '0'));
                     end if;
                  end if;
                  uart_buffer(uart_buffer_count)<=uart_rx_data;
                  uart_buffer_count := uart_buffer_count+1;
               END IF;

            WHEN config =>
               IF(uart_rx_busy = '0' AND uart_rx_busy_prev = '1'and uart_rx_error = '0' ) THEN    --new byte from uart is configuration
                  uart_stop_bits<= uart_rx_data(0);      --first bit correspond to number of stop bits 
                  uart_baud_rate<= uart_rx_data(3 downto 1); --2nd and 3rd to baud rate 
                  uart_parity_bits  <= uart_rx_data(5 downto 4); --4th and 5th to number and sort of parity
                  i2c_bus_clk  <= uart_rx_data(7 downto 6); --last 2 bits to i2c slaves speed
                  state <= ready;                          
               end if;

            WHEN i2c =>
               IF(uart_rx_busy = '0' AND uart_rx_busy_prev = '1' and uart_rx_error = '0') THEN --new byte from uart is i2c adress
                  if (to_integer(unsigned(uart_rx_data(6 downto 0))) > no_of_slaves) then
                     state <= ready;
                  else
                     i2c_addr(to_integer(unsigned(uart_rx_data(6 downto 0)))) <= uart_rx_data(6 downto 0); --1st 6 bits are adress
                     i2c_rw(to_integer(unsigned(uart_rx_data(6 downto 0))))   <= uart_rx_data(7);  --7th bit is read or write command (0 - write, 1 - read)
                     state <= i2c_command;
                  end if;
               end if;

            WHEN i2c_command =>
               IF(uart_rx_busy = '0' AND uart_rx_busy_prev = '1' and uart_rx_error = '0') THEN  --new byte from uart is data to be sent (or nothing if read)
                  i2c_data_wr(to_integer(unsigned(uart_rx_data_prev_addr))) <= uart_rx_data;
                  if (i2c_busy(to_integer(unsigned(uart_rx_data_prev_addr))) = '0') then  --if current i2c bus is busy, reject command
                     i2c_ena(to_integer(unsigned(uart_rx_data_prev_addr))) <= '1';
                  end if;
                  state <= ready;                          
               end if;
         END CASE;

         for i in 0 to no_of_slaves-1 loop
            if(i2c_accept(i) = '1') then     --if command is accepted by proper i2c bus, turn off enable
               i2c_ena(i)  <=  '0';
            end if;
         end loop;
         for i in 0 to no_of_slaves-1 loop
            if (i2c_finished(i) = '1' and current /= i and i2c_ack_err(i)='0') then --have valid read data that is currently not read by uart
               sent_flags(i) <= '1'; --put i2c slave data in waiting line
            end if;
         end loop;
         for i in 0 to no_of_slaves-1 loop
            if (sent_flags(i) = '1' and uart_tx_busy = '0' )then --there is data to send and uart is not busy
               current <= i;           --set current i2c slave
               uart_tx_data  <= i2c_data_rd(i); --send data that is read from i2c
               uart_tx_ena <= '1';     --enable uart sending
               sent_flags(i) <= '0';   --erase data from waiting line
               exit;
            else
               uart_tx_ena <= '0';     --disable uart sending
            end if;
         end loop;

      END IF;
   END PROCESS;


END behavior;
