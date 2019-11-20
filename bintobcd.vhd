----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:05:43 11/12/2019 
-- Design Name: 
-- Module Name:    btobcd - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;


entity bin2bcd is
    port(
        clk, reset: in std_logic;
        binary_in: in std_logic_vector(31 downto 0);
        bcd: out std_logic_vector(39 downto 0)
    );
end bin2bcd;
 
architecture behaviour of bin2bcd is
    type states is (start, shift, done);
    signal state, state_next: states;
 
    signal binary, binary_next: std_logic_vector(31 downto 0);
    signal bcds, bcds_reg, bcds_next: std_logic_vector(39 downto 0);
    -- output register keep output constant during conversion
    signal bcds_out_reg, bcds_out_reg_next: std_logic_vector(39 downto 0);
    -- need to keep track of shifts
    signal shift_counter, shift_counter_next: natural range 0 to 32;
begin
 
    process(clk, reset)
    begin
        if reset = '1' then
            binary <= (others => '0');
            bcds <= (others => '0');
            state <= start;
            bcds_out_reg <= (others => '0');
            shift_counter <= 0;
        elsif falling_edge(clk) then
            binary <= binary_next;
            bcds <= bcds_next;
            state <= state_next;
            bcds_out_reg <= bcds_out_reg_next;
            shift_counter <= shift_counter_next;
        end if;
    end process;
 
    convert:
    process(state, binary, binary_in, bcds, bcds_reg, shift_counter)
    begin
        state_next <= state;
        bcds_next <= bcds;
        binary_next <= binary;
       shift_counter_next <= shift_counter;

        case state is
            when start =>
                state_next <= shift;
                binary_next <= binary_in;
                bcds_next <= (others => '0');
                shift_counter_next <= 0;
            when shift =>
                if shift_counter = 32 then
                    state_next <= done;
                else
                    binary_next <= binary(30 downto 0) & 'L';
                    bcds_next <= bcds_reg(38 downto 0) & binary(31);
                    shift_counter_next <= shift_counter + 1;
                end if;
            when done =>
                state_next <= start;
        end case;
    end process;
 
    bcds_reg(39 downto 36) <= bcds(39 downto 36) + 3 when bcds(39 downto 36) > 4 else
                              bcds(39 downto 36);
    bcds_reg(35 downto 32) <= bcds(35 downto 32) + 3 when bcds(35 downto 32) > 4 else
                              bcds(35 downto 32);
    bcds_reg(31 downto 28) <= bcds(31 downto 28) + 3 when bcds(31 downto 28) > 4 else
                             bcds(31 downto 28);
    bcds_reg(27 downto 24) <= bcds(27 downto 24) + 3 when bcds(27 downto 24) > 4 else
                            bcds(27 downto 24);
    bcds_reg(23 downto 20) <= bcds(23 downto 20) + 3 when bcds(23 downto 20) > 4 else
                            bcds(23 downto 20);

    bcds_reg(19 downto 16) <= bcds(19 downto 16) + 3 when bcds(19 downto 16) > 4 else
                              bcds(19 downto 16);
    bcds_reg(15 downto 12) <= bcds(15 downto 12) + 3 when bcds(15 downto 12) > 4 else
                              bcds(15 downto 12);
    bcds_reg(11 downto 8) <= bcds(11 downto 8) + 3 when bcds(11 downto 8) > 4 else
                             bcds(11 downto 8);
    bcds_reg(7 downto 4) <= bcds(7 downto 4) + 3 when bcds(7 downto 4) > 4 else
                            bcds(7 downto 4);
    bcds_reg(3 downto 0) <= bcds(3 downto 0) + 3 when bcds(3 downto 0) > 4 else
                            bcds(3 downto 0);
     
    bcds_out_reg_next <= bcds when state = done else
                         bcds_out_reg;

    bcd(39 downto 36) <= bcds_out_reg(39 downto 36);
    bcd(35 downto 32) <= bcds_out_reg(35 downto 32);
    bcd(31 downto 28) <= bcds_out_reg(31 downto 28);
    bcd(27 downto 24) <= bcds_out_reg(27 downto 24);
    bcd(23 downto 20) <= bcds_out_reg(23 downto 20);
   
    bcd(19 downto 16) <= bcds_out_reg(19 downto 16);
    bcd(15 downto 12) <= bcds_out_reg(15 downto 12);
    bcd(11 downto 8) <= bcds_out_reg(11 downto 8);
    bcd(7 downto 4) <= bcds_out_reg(7 downto 4);
    bcd(3 downto 0) <= bcds_out_reg(3 downto 0);
 
end behaviour;