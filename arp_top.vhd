library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

-- Top level entity for the ARP block
-- 10/06/2019
-- By Grant Yu

-- This ARP block buffers in valid Ethernet frames, checking if they satisfy the criteria for an ARP request.
-- Once the request is received, a response is sent out. 
-- We have 2 concurrent processes controlled by state machines, to handle the request and the response.
-- The total simulation time (receive request, and fully send out response) takes 784 ns given RX and TX clocks of 125 MHz.

-- Software used: Quartus Prime Lite 18.1, ModelSim Intel FPGA Starter Edition 18.1

entity arp_top is 
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
end entity arp_top;

architecture rtl of arp_top is 

-- finite state machine for receiving APR requests
type req_state_type is 
(
    VALID_ARP_REQUEST, 
    INVALID_ARP_REQUEST,
    WAIT_ON_OUTPUT_BUFFER
);
-- FSM for sending ARP responses
type send_state_type is 
(
    WAIT_FOR_START, 
    WRITE_TO_FIFO
);
-- FSM to latch the DATA_ACK_TX signal
type ACK_state_type is
(
    LOW_STATE,
    LATCH_STATE
);
type mac_addr_array is array (5 downto 0) of std_logic_vector (7 downto 0);
type ip_addr_array is array (3 downto 0) of std_logic_vector (7 downto 0);
type resp_lut_type is array (0 to 9) of std_logic_vector (7 downto 0);
constant LUT_BYTES : integer := 8;
constant MAC_ADDR_BYTES : integer := 6;
constant IP_ADDR_BYTES : integer := 4;

-- pre-determined bytes to send out for the ARP response (ethernet frame, IPV4)
constant RESPONSE_CONSTS_LUT : resp_lut_type := 
(
    x"08", 
    x"06",
    x"00",
    x"01", 
    x"08", 
    x"00", 
    x"06", 
    x"04", 
    x"00", 
    x"02"
);
signal req_state, req_state_next : req_state_type := VALID_ARP_REQUEST;
signal send_state, send_state_next : send_state_type := WAIT_FOR_START;
signal ACK_state, ACK_state_next : ACK_state_type := LOW_STATE;
signal bytes_shift_reg, bytes_shift_reg_c : mac_addr_array := (others => (others => '0'));
signal src_ip_addr, src_ip_addr_c : ip_addr_array := (others => (others => '0'));
signal src_mac_addr, src_mac_addr_c : mac_addr_array := (others => (others => '0'));
signal incoming_byte_count, incoming_byte_count_c : std_logic_vector (5 downto 0) := (others => '0');
signal outgoing_byte_count, outgoing_byte_count_c : std_logic_vector (5 downto 0) := (others => '0');
signal burst, burst_c : std_logic := '0';
signal send : std_logic := '0';
signal data_fifo_in_rd_en, data_fifo_in_wr_en : std_logic := '0';
signal data_fifo_in_din, data_fifo_in_dout : std_logic_vector (7 downto 0) := (others => '0');
signal data_fifo_in_full, data_fifo_in_empty : std_logic := '0';
signal data_valid_fifo_in_rd_en, data_valid_fifo_in_wr_en : std_logic := '0';
signal data_valid_fifo_in_din, data_valid_fifo_in_dout : std_logic_vector (0 downto 0) := (others => '0');
signal data_valid_fifo_in_full, data_valid_fifo_in_empty : std_logic := '0';
signal data_fifo_out_rd_en, data_fifo_out_wr_en : std_logic := '0';
signal data_fifo_out_din, data_fifo_out_dout : std_logic_vector (7 downto 0) := (others => '0');
signal data_fifo_out_full, data_fifo_out_empty : std_logic := '0';


component fifo is
    generic
    (
        constant FIFO_DATA_WIDTH : integer;
        constant FIFO_BUFFER_SIZE : integer
    );
    port
    (
        signal rd_clk : in std_logic;
        signal wr_clk : in std_logic;
        signal reset : in std_logic;
        signal rd_en : in std_logic;
        signal wr_en : in std_logic;
        signal din : in std_logic_vector ((FIFO_DATA_WIDTH - 1) downto 0);
        signal dout : out std_logic_vector ((FIFO_DATA_WIDTH - 1) downto 0);
        signal full : out std_logic;
        signal empty : out std_logic
    );
end component fifo;
    

begin

    -- input frame buffering
    data_rx_fifo : fifo 
    generic map 
    (
        FIFO_DATA_WIDTH => 8,
        FIFO_BUFFER_SIZE => 256
    )
    port map 
    (
        rd_clk => CLK_RX,
        wr_clk => CLK_RX,
        reset => ARESET,
        rd_en => data_fifo_in_rd_en,
        wr_en => data_fifo_in_wr_en,
        din => data_fifo_in_din,
        dout => data_fifo_in_dout,
        full => data_fifo_in_full,
        empty => data_fifo_in_empty
    );

    data_valid_rx_fifo : fifo 
    generic map 
    (
        FIFO_DATA_WIDTH => 1,
        FIFO_BUFFER_SIZE => 256
    )
    port map 
    (
        rd_clk => CLK_RX,
        wr_clk => CLK_RX,
        reset => ARESET,
        rd_en => data_valid_fifo_in_rd_en,
        wr_en => data_valid_fifo_in_wr_en,
        din => data_valid_fifo_in_din,
        dout => data_valid_fifo_in_dout,
        full => data_valid_fifo_in_full,
        empty => data_valid_fifo_in_empty
    );

    -- response frame buffering
    data_tx_fifo : fifo 
    generic map 
    (
        FIFO_DATA_WIDTH => 8,
        FIFO_BUFFER_SIZE => 256
    )
    port map 
    (
        rd_clk => CLK_TX,
        wr_clk => CLK_TX,
        reset => ARESET,
        rd_en => data_fifo_out_rd_en,
        wr_en => data_fifo_out_wr_en,
        din => data_fifo_out_din,
        dout => data_fifo_out_dout,
        full => data_fifo_out_full,
        empty => data_fifo_out_empty
    );

    data_rx_write : process (data_fifo_in_full, DATA_RX)
    begin
        data_fifo_in_wr_en <= '0';
        data_fifo_in_din <= DATA_RX;
        if (data_fifo_in_full = '0') then
            data_fifo_in_wr_en <= '1';
        end if;
    end process;

    data_valid_rx_write : process (data_valid_fifo_in_full, DATA_VALID_RX)
    begin
        data_valid_fifo_in_wr_en <= '0';
        data_valid_fifo_in_din(0) <= DATA_VALID_RX;
        if (data_valid_fifo_in_full = '0') then
            data_valid_fifo_in_wr_en <= '1';
        end if;
    end process;

    data_tx_read : process (data_fifo_out_empty, data_fifo_out_dout, burst, DATA_ACK_TX)
    begin
        data_fifo_out_rd_en <= '0';
        DATA_TX <= data_fifo_out_dout;
        if (data_fifo_out_empty = '0') then
            DATA_VALID_TX <= '1';
        else
            DATA_VALID_TX <= '0';
        end if;
        if (data_fifo_out_empty = '0' and (burst = '1' or DATA_ACK_TX = '1')) then
            data_fifo_out_rd_en <= '1';
        end if;
    end process;

    arp_req_dff : process (ARESET, CLK_RX) 
    begin
        if (ARESET = '1') then  
            incoming_byte_count <= (others => '0');
            bytes_shift_reg <= (others => (others => '0'));
            req_state <= VALID_ARP_REQUEST;
            src_mac_addr <= (others => (others => '0'));
            src_ip_addr <= (others => (others => '0'));
        elsif (rising_edge(CLK_RX)) then
            incoming_byte_count <= incoming_byte_count_c;
            bytes_shift_reg <= bytes_shift_reg_c;
            req_state <= req_state_next;
            src_mac_addr <= src_mac_addr_c;
            src_ip_addr <= src_ip_addr_c;
        end if;
    end process;

    arp_send_dff : process (ARESET, CLK_TX)
    begin
        if (ARESET = '1') then
            send_state <= WAIT_FOR_START;
            outgoing_byte_count <= (others => '0');
            burst <= '0';
            ACK_state <= LOW_STATE;
        elsif (rising_edge(CLK_TX)) then
            send_state <= send_state_next;
            outgoing_byte_count <= outgoing_byte_count_c;
            burst <= burst_c;
            ACK_state <= ACK_state_next;
        end if;
    end process;

    -- FSM to handle the ARP request. Relies on a byte counter to keep track of where we are within the frame.
    parse_ARP_req_comb : process 
    (
        req_state, 
        src_mac_addr, 
        src_ip_addr,
        incoming_byte_count, 
        MY_IPV4,
        bytes_shift_reg,
        data_valid_fifo_in_empty,
        data_valid_fifo_in_dout,
        data_fifo_in_empty,
        data_fifo_in_dout,
        burst
    )
    begin

        req_state_next <= req_state;
        incoming_byte_count_c <= incoming_byte_count;
        src_mac_addr_c <= src_mac_addr;
        src_ip_addr_c <= src_ip_addr;
        send <= '0';
        data_valid_fifo_in_rd_en <= '0';
        data_fifo_in_rd_en <= '0';
        bytes_shift_reg_c <= bytes_shift_reg;

        case (req_state) is 
            -- parse the ethernet frame so long as it is a valid ARP request
            when VALID_ARP_REQUEST => 
                if (data_fifo_in_empty = '0') then
                    data_fifo_in_rd_en <= '1';
                    for i in 5 downto 1 loop 
                        bytes_shift_reg_c(i) <= bytes_shift_reg(i - 1);
                    end loop;
                    bytes_shift_reg_c(0) <= (others => '0');
                    if (data_valid_fifo_in_dout = "1") then
                        bytes_shift_reg_c(0) <= data_fifo_in_dout;
                    end if;
                end if;
                if (data_valid_fifo_in_empty = '0') then
                    data_valid_fifo_in_rd_en <= '1';
                    if (data_valid_fifo_in_dout = "1") then 
                        incoming_byte_count_c <= std_logic_vector(unsigned(incoming_byte_count) + 1);
                    end if;
                end if;
                if (((unsigned(incoming_byte_count) = 14) and (bytes_shift_reg(0) /= x"06") and (bytes_shift_reg(1) /= x"08")) or
                    ((unsigned(incoming_byte_count) = 22) and (bytes_shift_reg(0) /= x"01") and (bytes_shift_reg(1) /= x"00"))) then
                    incoming_byte_count_c <= (others => '0');
                    req_state_next <= INVALID_ARP_REQUEST; -- if Ethertype is not ARP or opcode is not "request" then invalid frame
                end if;
                if (unsigned(incoming_byte_count) = 28) then
                    for x in 5 downto 0 loop
                        src_mac_addr_c(5 - x) <= bytes_shift_reg(x); -- store, because needed for ARP response
                    end loop;
                end if;
                if (unsigned(incoming_byte_count) = 32) then
                    for y in 3 downto 0 loop
                        src_ip_addr_c(3 - y) <= bytes_shift_reg(y); -- store, because needed for ARP response
                    end loop;
                end if;
                if (unsigned(incoming_byte_count) = 42) then
                    incoming_byte_count_c <= (others => '0');
                    if (bytes_shift_reg(3) & bytes_shift_reg(2) & bytes_shift_reg(1) & bytes_shift_reg(0) = MY_IPV4) then
                        send <= '1'; -- once the IPV4 address in the request is a match, tell the ARP Response FSM to start
                    end if;
                    req_state_next <= WAIT_ON_OUTPUT_BUFFER; 
                end if;
            when INVALID_ARP_REQUEST =>
                if (data_fifo_in_empty = '0') then
                    data_fifo_in_rd_en <= '1'; -- empty the remaining bytes of the non-ARP request Ethernet frame
                end if;
                if (data_valid_fifo_in_empty = '0') then
                    data_valid_fifo_in_rd_en <= '1';
                    if (data_valid_fifo_in_dout = "0") then 
                        req_state_next <= VALID_ARP_REQUEST;  -- Assume DATA_VALID_RX has to go low before the next potential ARP request comes in, due to the FCS, Preamble, and SOF
                    end if;
                end if;
            when WAIT_ON_OUTPUT_BUFFER => -- parse more incoming ethernet frames only after the response for the last incoming ARP request started sending
                if (burst = '1') then
                    req_state_next <= VALID_ARP_REQUEST;
                end if;
            when others =>
                req_state_next <= VALID_ARP_REQUEST;
        end case;

    end process;

    -- FSM to control ARP response
    send_ARP_resp_comb : process 
    (
        send_state,
        send,
        src_mac_addr,
        src_ip_addr,
        outgoing_byte_count,
        MY_MAC,
        MY_IPV4,
        data_fifo_out_full
    )
    variable i : integer := 0;
    begin
        i := to_integer(unsigned(outgoing_byte_count));
        send_state_next <= send_state;
        outgoing_byte_count_c <= outgoing_byte_count;
        data_fifo_out_wr_en <= '0';
        data_fifo_out_din <= (others => '0');

        case (send_state) is
            when WAIT_FOR_START => -- once the ARP request has been parsed, send the ARP response
                if (send = '1') then
                    send_state_next <= WRITE_TO_FIFO;
                end if;
            when WRITE_TO_FIFO =>
                if (data_fifo_out_full = '0') then
                    data_fifo_out_wr_en <= '1';
                    outgoing_byte_count_c <= std_logic_vector(unsigned(outgoing_byte_count) + 1);
                    if (unsigned(outgoing_byte_count) = 41) then
                        send_state_next <= WAIT_FOR_START;
                    end if;
                end if;
                -- select the data to buffer in the ARP response
                case (i) is
                    when 0 to 5 =>
                        data_fifo_out_din <= src_mac_addr(i);
                    when 6 to 11 =>
                        data_fifo_out_din <= MY_MAC(47 - (8 * (i - 6)) downto 40 - (8 * (i - 6)));
                    when 12 to 21 =>
                        data_fifo_out_din <= RESPONSE_CONSTS_LUT(i - 12);
                    when 22 to 27 =>
                        data_fifo_out_din <= MY_MAC(47 - (8 * (i - 22)) downto 40 - (8 * (i - 22)));
                    when 28 to 31 =>
                        data_fifo_out_din <= MY_IPV4(31 - (8 * (i - 28)) downto 24 - (8 * (i - 28)));
                    when 32 to 37 =>
                        data_fifo_out_din <= src_mac_addr(i - 32);
                    when 38 to 41 =>
                        data_fifo_out_din <= src_ip_addr(i - 38);
                    when others =>
                        send_state_next <= WAIT_FOR_START;
                end case;
            when others =>
                send_state_next <= WAIT_FOR_START;
        end case;
    end process;

    -- since the DATA_ACK_TX is high for only 1 clock cycle, a 2-state FSM must be used to latch the data for the duration of 
    -- the ARP response. 
    latch_ACK_signal : process 
    (
        ACK_state,
        DATA_ACK_TX,
        burst,
        data_fifo_out_empty
    )
    begin
        ACK_state_next <= ACK_state;
        burst_c <= burst;
        case (ACK_state) is
            when LOW_STATE =>
                burst_c <= '0';
                if (DATA_ACK_TX = '1') then
                    ACK_state_next <= LATCH_STATE;
                    burst_c <= '1';
                end if;
            when LATCH_STATE =>
                if (data_fifo_out_empty = '1') then
                    burst_c <= '0';
                    ACK_state_next <= LOW_STATE;
                end if;
            when others => 
                ACK_state_next <= LOW_STATE;
        end case;
    end process;

end architecture rtl;