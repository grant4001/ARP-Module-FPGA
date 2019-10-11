library IEEE;
library std;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_textio.all;
use STD.textio.all;

-- Testbench for the ARP block
-- 10/06/2019
-- By Grant Yu

-- Use a test input "test_input.txt" containing a valid ARP request.
-- Write the output to "test_output.txt", while comparing the result with "compare.txt" which is the right ARP response.
-- The cycle count, simulation time, and error count are written out to the ModelSim console output.

entity arp_tb is
generic (
    constant RX_PERIOD : time := 8 ns;
    constant TX_PERIOD : time := 8 ns;
    constant ACK_TX_DELAY : integer := 7;
    constant IN_FILE_NAME : string (14 downto 1) := "test_input.txt";
    constant OUT_FILE_NAME : string (15 downto 1) := "test_output.txt";
    constant CMP_FILE_NAME : string (11 downto 1) := "compare.txt"
);
end entity arp_tb;

architecture behavior of arp_tb is

    component arp_top is 
    port
    (
        ARESET : in std_logic;
        MY_MAC : in std_logic_vector (47 downto 0);
        MY_IPV4 : in std_logic_vector (31 downto 0);
        CLK_RX : in std_logic;
        DATA_VALID_RX : in std_logic;
        DATA_RX : in std_logic_vector (7 downto 0);
        CLK_TX : in std_logic;
        DATA_VALID_TX : out std_logic;
        DATA_TX : out std_logic_vector (7 downto 0);
        DATA_ACK_TX : in std_logic
    );
    end component arp_top;

    function to_slv(c : character) return std_logic_vector is
        begin
            return std_logic_vector(to_unsigned(character'pos(c),8));
        end function to_slv;
        
    function to_char(v : std_logic_vector) return character is
    begin
        return character'val(to_integer(unsigned(v)));
    end function to_char;

    signal CLK_RX_t : std_logic := '1';
    signal CLK_TX_t : std_logic := '1';
    signal ARESET_t : std_logic := '0';
    signal MY_MAC_t : std_logic_vector (47 downto 0) := (others => '0');
    signal MY_IPV4_t : std_logic_vector (31 downto 0) := (others => '0');
    signal DATA_VALID_RX_t : std_logic := '0';
    signal DATA_RX_t : std_logic_vector (7 downto 0) := (others => '0');
    signal DATA_VALID_TX_t : std_logic := '0';
    signal DATA_TX_t : std_logic_vector (7 downto 0) := (others => '0');
    signal DATA_ACK_TX_t : std_logic := '0';
    signal hold_clock : std_logic := '0';
    signal in_write_done : std_logic := '0';
    signal out_read_done : std_logic := '0';
    signal out_errors : integer := 0;

    begin

    arp_top_inst : arp_top 
    port map (
        ARESET => ARESET_t,
        MY_MAC => MY_MAC_t,
        MY_IPV4 => MY_IPV4_t,
        CLK_RX => CLK_RX_t,
        DATA_VALID_RX => DATA_VALID_RX_t,
        DATA_RX => DATA_RX_t,
        CLK_TX => CLK_TX_t,
        DATA_VALID_TX => DATA_VALID_TX_t,
        DATA_TX => DATA_TX_t,
        DATA_ACK_TX => DATA_ACK_TX_t
    );

    rx_clock_process : process
    begin
        CLK_RX_t <= '1';
        wait for  (RX_PERIOD / 2);
        CLK_RX_t <= '0';
        wait for  (RX_PERIOD / 2);
        if ( hold_clock = '1' ) then
            wait;
        end if;
    end process rx_clock_process;

    tx_clock_process : process
    begin
        CLK_TX_t <= '1';
        wait for  (TX_PERIOD / 2);
        CLK_TX_t <= '0';
        wait for  (TX_PERIOD / 2);
        if ( hold_clock = '1' ) then
            wait;
        end if;
    end process tx_clock_process;

    reset_process : process
    begin
        ARESET_t <= '0';
        wait until  (CLK_RX_t = '0');
        wait until  (CLK_RX_t = '1');
        ARESET_t <= '1';
        wait until  (CLK_RX_t = '0');
        wait until  (CLK_RX_t = '1');
        ARESET_t <= '0';
        wait;
    end process reset_process;

    inject_process : process 
        file in_file : text;
        variable ln1 : line;
        variable in_line : line;
        variable in_byte : std_logic_vector (7 downto 0) := (others => '0');
        variable i: integer := 0;
    begin
        wait until (ARESET_t = '1');
        wait until (ARESET_t = '0');
        wait until  (CLK_RX_t = '0');
        wait until  (CLK_RX_t = '1');
    
        write( ln1, string'("@ ") );
        write( ln1, NOW );
        write( ln1, string'(": Loading file ") );
        write( ln1, IN_FILE_NAME );
        write( ln1, string'("...") );
        writeline( output, ln1 );
        file_open( in_file, IN_FILE_NAME, read_mode );

        MY_MAC_t <= x"00" & x"02" & x"23" & x"01" & x"02" & x"03";
        MY_IPV4_t <= x"c0" & x"a8" & x"01" & x"02";

        while ( not ENDFILE(in_file) ) loop
            wait until (CLK_RX_t = '0');
            wait until (CLK_RX_t = '1');
            readline(in_file, in_line);
            hread(in_line, in_byte);
            DATA_RX_t <= in_byte;
            DATA_VALID_RX_t <= '1';
        end loop;

        wait until (CLK_RX_t = '0');
        wait until (CLK_RX_t = '1');
        DATA_RX_t <= (others => '0');
        DATA_VALID_RX_t <= '0';
        file_close( in_file );
        in_write_done <= '1';
        wait;
    end process inject_process; 

    file_write_process : process 
        file out_file : text;
        file cmp_file : text;
        variable ln1, ln2, ln3, ln4 : line;
        variable out_data_cmp : std_logic_vector (7 downto 0);
        variable cmp_data : std_logic_vector (7 downto 0);
        variable i, j : integer := 0;
    begin
        wait until  (ARESET_t = '1');
        wait until  (ARESET_t = '0');
        wait until  (CLK_TX_t = '0');
        wait until  (CLK_TX_t = '1');

        write( ln1, string'("@ ") );
        write( ln1, NOW );
        write( ln1, string'(": Comparing with file ") );
        write( ln1, CMP_FILE_NAME );
        write( ln1, string'("...") );
        writeline( output, ln1 );
        file_open(out_file, OUT_FILE_NAME, write_mode);
        file_open(cmp_file, CMP_FILE_NAME, read_mode);

        while ( not ENDFILE(cmp_file) ) loop
            wait until ( CLK_TX_t = '0');
            wait until ( CLK_TX_t = '1');
            if ( DATA_VALID_TX_t = '1' ) then
                j := j + 1;
                if (j = ACK_TX_DELAY) then
                    DATA_ACK_TX_t <= '1';
                else
                    DATA_ACK_TX_t <= '0';
                end if;
                if (j > ACK_TX_DELAY) then
                    hwrite( ln2, DATA_TX_t );
                    writeline( out_file, ln2 );
                    readline( cmp_file, ln3 );
                    hread( ln3, cmp_data );
                    if ( to_01(unsigned(DATA_TX_t)) /= to_01(unsigned(cmp_data)) ) then
                        out_errors <= out_errors + 1;
                        write( ln2, string'("@ ") );
                        write( ln2, NOW );
                        write( ln2, string'(": ") );
                        write( ln2, OUT_FILE_NAME );
                        write( ln2, string'("(") );
                        write( ln2, i + 1 );
                        write( ln2, string'("): ERROR: ") );
                        hwrite( ln2, DATA_TX_t );
                        write( ln2, string'(" != ") );
                        hwrite( ln2, cmp_data);
                        write( ln2, string'(" at address 0x") );
                        hwrite( ln2, std_logic_vector(to_unsigned(i, 32)) );
                        write( ln2, string'(".") );
                        writeline( output, ln2 );
                    end if;
                end if;
            end if;
            i := i + 1;
        end loop;
        wait until  (CLK_TX_t = '0');
        wait until  (CLK_TX_t = '1');
		DATA_ACK_TX_t <= '0';
        file_close( out_file );
        out_read_done <= '1';
        wait;
    end process file_write_process;

    --Main testbench process
    tb_proc : process
        variable errors : integer := 0;
        variable warnings : integer := 0;
        variable start_time : time;
        variable end_time : time;
        variable ln1, ln2, ln3, ln4, ln5 : line;
    begin
        wait until  (ARESET_t = '1');
        wait until  (ARESET_t = '0');
        wait until  (CLK_RX_t = '0');
        wait until  (CLK_RX_t = '1');

        start_time := NOW;
        write( ln1, string'("@ ") );
        write( ln1, start_time );
        write( ln1, string'(": Beginning simulation...") );
        writeline( output, ln1 );

        wait until  (CLK_TX_t = '0');
        wait until  (CLK_TX_t = '1');
        wait until (out_read_done = '1');

        end_time := NOW;
        write( ln2, string'("@ ") );
        write( ln2, end_time );
        write( ln2, string'(": Simulation completed.") );
        writeline( output, ln2 );
        errors := out_errors;

        write( ln3, string'("Total simulation time: ") );
        write( ln3, (end_time - start_time) );
        writeline( output, ln3 );

        write( ln4, string'("Total simulation cycle count: ") );
        write( ln4, (end_time - start_time) / RX_PERIOD );
        writeline( output, ln4 );

        write( ln5, string'("Total error count: ") );
        write( ln5, errors );
        writeline( output, ln5 );
        
        hold_clock <= '1';
        wait;
    end process tb_proc;

end architecture behavior;
