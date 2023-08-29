
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

ENTITY i2c IS
   GENERIC(
      input_clk : INTEGER := 50_000_000); --input clock speed from user logic in Hz
   PORT(
      bus_clk   : in     STD_LOGIC_VECTOR(1 DOWNTO 0);
      clk       : IN     STD_LOGIC;                    --system clock
      reset_n   : IN     STD_LOGIC;                    --active low reset
      ena       : IN     STD_LOGIC;                    --latch in command
      addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
      rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
      data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
      busy      : OUT    STD_LOGIC;                    --work in progress
      accept      : OUT    STD_LOGIC;                  --command accepted 
      finish      : OUT    STD_LOGIC;                  --indicates transaction in progress
      data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
      ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
      sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
      scl       : INOUT  STD_LOGIC);                   --serial clock output of i2c bus
END i2c;

ARCHITECTURE logic OF i2c IS
   signal i2c_clk : integer := 100000;
   signal divider  :  INTEGER ; --number of clocks in 1/4 cycle of scl
   TYPE machine IS(ready, start, command, slv_ack1, wr, rd, slv_ack2, mstr_ack, stop); --needed states
   SIGNAL state         : machine;                        --state machine
   SIGNAL data_clk      : std_logic;                      --data clock for sda
   SIGNAL data_clk_prev : std_logic;                      --data clock during previous system clock
   SIGNAL scl_clk       : std_logic;                      --constantly running internal scl
   SIGNAL scl_ena       : std_logic := '0';               --enables internal scl to output
   SIGNAL sda_int       : std_logic := '1';               --internal sda
   SIGNAL sda_ena_n     : std_logic;                      --enables internal sda to output
   SIGNAL addr_rw       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --latched in address and read/write
   SIGNAL data_tx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --latched in data to write to slave
   SIGNAL data_rx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --data received from slave
   SIGNAL bit_cnt       : INTEGER RANGE 0 TO 7 := 7;      --tracks bit number in transaction
   SIGNAL stretch       : std_logic := '0';               --identifies if slave is stretching scl

BEGIN

   divider  <=(input_clk/i2c_clk)/4;      --number of clocks in 1/4 cycle of scl
   process (bus_clk)                      --change of i2c slave speed depending on bus_clk input
   begin
      case bus_clk is
         when "00" =>
            i2c_clk <= 100000;
         when "01" =>
            i2c_clk <= 400000;
         when "10" =>
            i2c_clk <= 500000;
         when "11" =>
            i2c_clk <= 1000000;
         when others =>
            i2c_clk <= i2c_clk;
      end case;
   end process;
   --generate the timing for the bus clock (scl_clk) and the data clock (data_clk)
   PROCESS(clk, reset_n)
      VARIABLE count  :  integer;  --timing for clock generation
   BEGIN
      IF(reset_n = '0') THEN                --reset asserted
         stretch <= '0';
         count := 0;
         scl_clk <= '0';
         data_clk <= '0';
      ELSIF(clk'EVENT AND clk = '1') THEN
         data_clk_prev <= data_clk;          --store previous value of data clock
         IF(count >= divider*4-1) THEN        --end of timing cycle
            count := 0;                       --reset timer
         ELSIF(stretch = '0') THEN           --clock stretching from slave not detected
            count := count + 1;               --continue clock generation timing
         END IF;
         if count<= divider-1 then
            scl_clk <= '0';
            data_clk <= '0';
         elsif count <= divider*2-1 then
            scl_clk <= '0';
            data_clk <= '1';
         elsif count <= divider*3-1 then
            scl_clk <= '1';                 --release scl
            IF(scl = '0') THEN              --detect if slave is stretching clock
               stretch <= '1';
            ELSE
               stretch <= '0';
            END IF;
            data_clk <= '1';
         else
            scl_clk <= '1';
            data_clk <= '0';
         end if;
      END IF;
   END PROCESS;

   --state machine and writing to sda during scl low (data_clk rising edge)
   PROCESS(clk, reset_n)
   BEGIN
      IF(reset_n = '0') THEN                 --reset asserted
         state <= ready;                      --return to initial state
         busy <= '1';                         --indicate not available
         accept  <= '0';
         finish<= '0';
         scl_ena <= '0';                      --sets scl high impedance
         sda_int <= '1';                      --sets sda high impedance
         ack_error <= '0';                    --clear acknowledge error flag
         bit_cnt <= 7;                        --restarts data bit counter
         data_rd <= (OTHERS => '0');              --clear data read port
         addr_rw  <= (OTHERS => '0');
         data_tx  <= (OTHERS => '0');
         data_rx <= (OTHERS => '0');
      ELSIF(clk'EVENT AND clk = '1') THEN
         IF(data_clk = '1' AND data_clk_prev = '0') THEN  --data clock rising edge
            CASE state IS
               WHEN ready =>                      --idle state
                  IF(ena = '1') THEN               --transaction requested
                     accept  <= '1';               --command accepted
                     busy <= '1';                   --flag busy
                     addr_rw <= addr & rw;          --collect requested slave address and command
                     data_tx <= data_wr;            --collect requested data to write
                     state <= start;                --go to start bit
                  ELSE                             --remain idle
                     busy <= '0';                   --unflag busy
                     state <= ready;                --remain idle
                  END IF;
               WHEN start =>                      --start bit of transaction
                  accept  <= '0';
                  busy <= '1';                     --resume busy if continuous mode
                  sda_int <= addr_rw(bit_cnt);     --set first address bit to bus
                  state <= command;                --go to command
               WHEN command =>                    --address and command byte of transaction
                  IF(bit_cnt = 0) THEN             --command transmit finished
                     sda_int <= '1';                --release sda for slave acknowledge
                     bit_cnt <= 7;                  --reset bit counter for "byte" states
                     state <= slv_ack1;             --go to slave acknowledge (command)
                  ELSE                             --next clock cycle of command state
                     bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
                     sda_int <= addr_rw(bit_cnt-1); --write address/command bit to bus
                     state <= command;              --continue with command
                  END IF;
               WHEN slv_ack1 =>                   --slave acknowledge bit (command)
                  IF(addr_rw(0) = '0') THEN        --write command
                     sda_int <= data_tx(bit_cnt);   --write first bit of data
                     state <= wr;                   --go to write byte
                  ELSE                             --read command
                     sda_int <= '1';                --release sda from incoming data
                     state <= rd;                   --go to read byte
                  END IF;
               WHEN wr =>                         --write byte of transaction
                  busy <= '1';                     --resume busy if continuous mode
                  IF(bit_cnt = 0) THEN             --write byte transmit finished
                     sda_int <= '1';                --release sda for slave acknowledge
                     bit_cnt <= 7;                  --reset bit counter for "byte" states
                     state <= slv_ack2;             --go to slave acknowledge (write)
                  ELSE                             --next clock cycle of write state
                     bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
                     sda_int <= data_tx(bit_cnt-1); --write next bit to bus
                     state <= wr;                   --continue writing
                  END IF;
               WHEN rd =>                         --read byte of transaction
                  busy <= '1';                     --resume busy if continuous mode
                  IF(bit_cnt = 0) THEN             --read byte receive finished                       
                     sda_int <= '1';              --send a no-acknowledge (before stop or repeated start)
                     bit_cnt <= 7;                  --reset bit counter for "byte" states
                     data_rd <= data_rx;            --output received data
                     state <= mstr_ack;             --go to master acknowledge
                  ELSE                             --next clock cycle of read state
                     bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
                     state <= rd;                   --continue reading
                  END IF;

               WHEN slv_ack2 =>                   
                  busy <= '0';                   --clear work in progress
                  state <= stop;                 --go to stop bit

               WHEN mstr_ack =>                   
                  finish<= '1';
                  busy <= '0';                    --clear work in progress                            
                  state <= stop;                  --go to stop bit
               WHEN stop =>
                  finish<= '0';                   --command finished
                  state <= ready;                 --go to idle state
            END CASE;
         ELSIF(data_clk = '0' AND data_clk_prev = '1') THEN  --data clock falling edge
            CASE state IS
               WHEN start =>
                  IF(scl_ena = '0') THEN                  --starting new transaction
                     scl_ena <= '1';                       --enable scl output
                     ack_error <= '0';                     --reset acknowledge error output
                  END IF;
               WHEN slv_ack1 =>                          --receiving slave acknowledge (command)
                  IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
                     ack_error <= '1';                     --set error output if no-acknowledge
                  END IF;
               WHEN rd =>                                --receiving slave data
                  data_rx(bit_cnt) <= sda;                --receive current slave data bit
               WHEN slv_ack2 =>                          --receiving slave acknowledge (write)
                  IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
                     ack_error <= '1';                     --set error output if no-acknowledge
                  END IF;
               WHEN stop =>
                  scl_ena <= '0';                         --disable scl
               WHEN OTHERS =>
                  NULL;
            END CASE;
         END IF;
      END IF;
   END PROCESS;

   --set sda output
   WITH state SELECT
 sda_ena_n <= data_clk_prev WHEN start,     --generate start condition
      NOT data_clk_prev WHEN stop,  --generate stop condition
      sda_int WHEN OTHERS;          --set to internal sda signal    

   --set scl and sda outputs
   scl <= '0' WHEN (scl_ena = '1' AND scl_clk = '0') ELSE 'Z';
   sda <= '0' WHEN sda_ena_n = '0' ELSE 'Z';

END logic;
