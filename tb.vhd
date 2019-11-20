--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   23:24:44 11/19/2019
-- Design Name:   
-- Module Name:   F:/Calc/tb.vhd
-- Project Name:  Calc
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: IO
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY tb IS
END tb;
 
ARCHITECTURE behavior OF tb IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT IO
    PORT(
	   
		  clk1 : in std_logic;
		  
         bcd_in : in std_logic_vector(3 downto 0);
			ans : out std_logic_vector(31 downto 0);
			sout : out std_logic;
			oflow: out std_logic
        );
    END COMPONENT;
    
   -- No clocks detected in port list. Replace <clock> below with 
   -- appropriate port name 
      
signal	     clk1 :  std_logic;

      signal   bcd_in : std_logic_vector(3 downto 0);
	signal		ans :  std_logic_vector(31 downto 0);
  signal	sout :  std_logic;
		signal	oflow: std_logic;
   
	constant clk1_period : time := 500 ps;
	
 
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: IO PORT MAP (
	  
		clk1=>clk1,
	     bcd_in => bcd_in,
			ans => ans,
			oflow=>oflow,
			sout=>sout
        );

   -- Clock process definitions
  
	clk1_process :process
   begin
		clk1 <= '0';
		wait for clk1_period/2;
		clk1 <= '1';
		wait for clk1_period/2;
   end process;

   -- Stimulus process
   stim_proc: process 
   begin		
	 -- if(rising_edge(clk)) then
      -- hold reset state for 100 ns.
   	wait for 100 ns;  
     
    	  bcd_in<="1010";
		wait for 50 ns;	
        bcd_in<="0011";
		 wait for 50 ns;	
         bcd_in<="0101";		
		wait for 50 ns;	
        bcd_in<="0011";  
	   wait for 50 ns;	
         bcd_in<="0101";		
	 	 wait for 50 ns;	
   
    	bcd_in<="1100";         
        wait for 50 ns;
        bcd_in<="1111";
    	wait for 1 ns;	
    

      bcd_in<="1010";
	 	 wait for 50 ns;	
        bcd_in<="0011"; 
		 wait for 50 ns;	
        bcd_in<="0111";
		 wait for 50 ns;	
        bcd_in<="0101";
--		 wait for 100 ns;	
--        bcd_in<="0101";
   wait;		
	--	end if;
	 
      -- insert stimulus here 

      
   end process;

END;
