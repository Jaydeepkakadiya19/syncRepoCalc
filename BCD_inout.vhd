----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    12:42:06 11/18/2019 
-- Design Name: 
-- Module Name:    BCD_inout - Behavioral 
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
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use ieee.numeric_std.all;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity BCD_inout is
port (
			clk: in std_logic;
			a : in std_logic_vector(31 downto 0);
			b : in std_logic_vector(31 downto 0);
			sin1 : in std_logic ;
			sin2 : in std_logic ;
			op   : in std_logic_vector(1 downto 0);
			bcdout : out std_logic_vector(31 downto 0);
			sinout : out std_logic;
			oflow : out std_logic 
);
			
end BCD_inout;

architecture Behavioral of bcd_inout is

		--------component calling_mod is
		--------
		--------    Port ( a : inout  STD_LOGIC_VECTOR (31 downto 0);
		--------           lca : in  STD_LOGIC_VECTOR (3 downto 0);
		--------           rca : in  STD_LOGIC_VECTOR (3 downto 0);
		--------			  asin : in std_logic;
		--------           b : inout  STD_LOGIC_VECTOR (31 downto 0);
		--------           lcb : in STD_LOGIC_VECTOR (3 downto 0);
		--------           rcb : in STD_LOGIC_VECTOR (3 downto 0);
		--------			  bsin : in std_logic;
		--------			  c: inout STD_LOGIC_VECTOR(31 downto 0);
		--------			  op: in STD_LOGIC_VECTOR (1 downto 0);
		--------			  sin : out std_logic;
		--------			  fp : out std_logic_vector(3 downto 0 );
		--------			  oflow : out std_logic
		--------			  );
		--------			  
		--------
		--------end component;

		--component bcd_2_bin is
		--    Port ( bcd_in_0 : in  STD_LOGIC_VECTOR (3 downto 0);
		--           bcd_in_10 : in  STD_LOGIC_VECTOR (3 downto 0);
		--           bcd_in_100 : in  STD_LOGIC_VECTOR (3 downto 0);
		--           bcd_in_1000 : in  STD_LOGIC_VECTOR (3 downto 0);
		--			  bcd_in_10000 : in STD_LOGIC_VECTOR (3 downto 0);  
		--			  bcd_in_100000 : in STD_LOGIC_VECTOR (3 downto 0);
		--			  bcd_in_1000000 : in STD_LOGIC_VECTOR (3 downto 0);
		--           bin_out : out  STD_LOGIC_VECTOR (31 downto 0) );
		--end component;


------  Binary to BCD (Double Dable) ------------------
		component bin2bcd is
			port(
				clk, reset: in std_logic;
				binary_in: in std_logic_vector(31 downto 0);
				bcd: out std_logic_vector(39 downto 0)
			);
		end component;
		
		component mult64 is
			Port ( --rst:in std_logic; 
				a : in std_logic_vector(31 downto 0);
				b : in std_logic_vector(31 downto 0);
				prod : out std_logic_vector(31 downto 0));
		end component;

------  Addition of two 32-bit numbers ------------------

		function adder(a: std_logic_vector(31 downto 0); b: std_logic_vector(31 downto 0); cin: std_logic) return std_logic_vector is

			variable x: std_logic:=cin;
			variable s: std_logic_vector(32 downto 0);

			begin

			FOR i in 0 to 31 loop
				s(i):= a(i) XOR b(i) XOR x;
				x := (a(i) AND b(i)) OR ((a(i) XOR b(i)) AND x);
			end loop;
			s(32):=x;
			return s;
		end adder;
		
------  Addition of two 64-bit numbers ------------------		
		
		function adder64(a: std_logic_vector(63 downto 0); b: std_logic_vector(63 downto 0); cin: std_logic) return std_logic_vector is

			variable x: std_logic:=cin;
			variable s: std_logic_vector(64 downto 0);

			begin

			FOR i in 0 to 63 loop
				s(i):= a(i) XOR b(i) XOR x;
				x := (a(i) AND b(i)) OR ((a(i) XOR b(i)) AND x);
			end loop;
			s(64):=x;
			return s;
		end adder64;


------  Subtraction of two numbers ------------------

		function sub(a : STD_LOGIC_VECTOR (31 downto 0);
					  b : STD_LOGIC_VECTOR (31 downto 0)) return std_logic_vector is
					  
			variable cout : STD_LOGIC;
			variable s : STD_LOGIC_VECTOR (31 downto 0);
			variable x: std_logic_vector(31 downto 0);
			variable y: std_logic_vector(31 downto 0):=not b;
			variable temp1,temp2,ans : std_logic_vector(32 downto 0);
			variable comp,comp2,comp3,comp4: std_logic_vector(31 downto 0);
			variable add1: std_logic_vector(31 downto 0);
			variable co,ci,co2 : std_logic;
			begin 
				comp:=not b;
				add1:="00000000000000000000000000000000";
				temp1:=adder(comp,a,'1');
				comp2:=temp1(31 downto 0);
				co:=temp1(32);
				comp4:= not comp2;
				temp2:=adder(comp4,add1,'1');
				comp3:=temp2(31 downto 0);
				co2:=temp2(32);
				
				if co='1' then
					ans(31 downto 0):=comp2;
					ans(32):='0';
				else 
					ans(31 downto 0):=comp3;
					ans(32):='1';
				end if;
				return ans;
		end sub;


------  Division of two numbers ------------------

		function div(a : std_logic_Vector(31 downto 0); b: std_logic_vector(31 downto 0)) return std_logic_vector is
			variable x,y: integer;
			variable ans : std_logic_vector(31 downto 0) := "00000000000000000000000000000000";
			variable temp1,temp2: integer ;
			variable result :  std_logic_vector (31 downto 0);
			begin
			x := CONV_INTEGER(a);
			y := CONV_INTEGER(b);
			
			temp1:=x;
			temp2:=y;
			
			for i in 23 downto 0 loop
				if (temp1>=temp2 * 2**i) then
					result(i):= '1';
					temp1:= temp1- temp2 * 2**i;
				else result(i):= '0';
				end if;
			end loop;
			result(31 downto 24):="00000000";
			return result;
		end div;
		
------  Left shifter function for 16 bit number ------------------		
		function left_shift(a:std_logic_vector(15 downto 0);b:integer) RETURN std_logic_vector IS
			variable s : std_logic_vector(15 downto 0):="0000000000000000";
			begin
				for i in b to 15 loop
					s(i):=a(i-b);
				end loop;
			return s;
		end left_shift;
		
------  Left shifter function for 32-bit number ------------------
		function left_shift32(a:std_logic_vector(31 downto 0); b: integer) return std_logic_vector is
			variable s: std_logic_vector(31 downto 0):="00000000000000000000000000000000";
			begin
				for i in b to 31 loop
					s(i):=a(i-b);
				end loop;
			return s;
		end left_shift32;
		
		
		
		
------  Right shifter for 32 bit number ------------------
		function right_shifter(a:std_logic_vector(31 downto 0);b:integer) RETURN std_logic_vector IS
			variable s : std_logic_vector(31 downto 0):="00000000000000000000000000000000";
			begin
				for i in 31 downto b loop
					s(i-b):=a(i);
				end loop;
			return s;
		end right_shifter;
		
		
------  Left shifter function for 26 bit number ------------------
		function left_shift26(a:std_logic_vector(25 downto 0); b: integer) return std_logic_vector is
			variable s: std_logic_vector(25 downto 0):="00000000000000000000000000";
			begin
				for i in 0 to 25-b loop
					s(i+b):=a(i);
				end loop;
			return s;
		end left_shift26;
		
		
------  Multiplication of two 32 bit numbers ------------------
		function mult(a: std_logic_vector(31 downto 0);b : std_logic_vector(31 downto 0)) return std_logic_vector is
			variable ans : std_logic_vector(31 downto 0);
			variable x : std_logic_vector(25 downto 0) := a(25 downto 0);
			variable y : std_logic_vector(25 downto 0) := b(25 downto 0);
			variable ss,temp : std_logic_vector(31 downto 0) :="00000000000000000000000000000000";
			variable temp_var: std_logic_vector(32 downto 0);
			begin
				ans := "00000000000000000000000000000000";
				for i in 0 to 25 loop
					if(y(i) = '1') then
						ss(25 downto 0) :=left_shift26(x,i);
						--z:=temp(25 downto 0);
						temp_var := (adder(ans,ss,'0'));
						ans := temp_var(31 downto 0);
					end if;
				end loop;
			return ans;
		end mult;
		
--		function mult(a: std_logic_vector(31 downto 0);b : std_logic_vector(31 downto 0)) return std_logic_vector is
--			variable s : std_logic_vector(63 downto 0);
--			variable x : std_logic_vector(31 downto 0) := a;
--			variable y : std_logic_vector(31 downto 0) := b;
--			variable z : std_logic_vector(63 downto 0) :="0000000000000000000000000000000000000000000000000000000000000000";
--			variable dumm: std_logic_vector(63 downto 0);
--			begin
--			s :="0000000000000000000000000000000000000000000000000000000000000000";
--			for i in 0 to 31 loop
--			if(y(i) = '1') then
--			z(15 downto 0) :=LEFT_SHIFT(x,i);
--			dumm := (adder64(s,z,'0'));
--			s := dumm(31 downto 0);
--			end if;
--			end loop;
--			return s;
--		end mult;
		
--		function newmult(a: std_logic_vector(31 downto 0); b: std_logic_vector(31 downto 0)) return std_logic_vector is
--			--variable out : std_logic_vector(63 downto 0);
--			variable prod : std_logic_vector(31 downto 0);			
--			constant n:integer :=16;
--			subtype plary is std_logic_vector(n-1 downto 0);
--			type pary is array(0 to n) of plary;
--			variable pp,pc,ps:pary;
--			begin
--				pgen:for j in 0 to n-1 generate
--				pgen1:for k in 0 to n-1 generate
--							pp(j)(k):=a(k) and b(j);
--						end generate;
--						pc(0)(j):='0';
--					end generate;
--
--				ps(0):=pp(0);
--				prod(0):=pp(0)(0);
--				addr:for j in 1 to n-1 generate
--				addc:for k in 0 to n-2 generate
--			ps(j)(k):=pp(j)(k) xor pc(j-1)(k) xor ps(j-1)(k+1);
--			pc(j)(k):=(pp(j)(k) and pc(j-1)(k)) or
--		  (pp(j)(k) and ps(j-1)(k+1)) or
--		  (pc(j-1)(k)and ps(j-1)(k+1));
--		end generate;
--		prod(j):=ps(j)(0);
--		ps(j)(n-1):=pp(j)(n-1); 
--	end generate;
--
--pc(n)(0):='0';
--addlast:for k in 1 to n-1 generate
--	ps(n)(k):=pc(n)(k-1) xor pc(n-1)(k-1) xor ps(n-1)(k);
--		pc(n)(k):=(pc(n)(k-1) and pc(n-1)(k-1)) or
--	   (pc(n)(k-1) and ps(n-1)(k)) or
--		(pc(n-1)(k-1)and ps(n-1)(k));
--	end generate;
--prod(2*n-1):=pc(n)(n-1);
--prod(2*n-2 downto n):=ps(n)(n-1 downto 1);
--	return prod;
--		end newmult;

------  BCD to binary  ------------------
		function bcd_2_bin(bcd_in_0: STD_LOGIC_VECTOR (3 downto 0);
					  bcd_in_10 : STD_LOGIC_VECTOR (3 downto 0);
					  bcd_in_100 : STD_LOGIC_VECTOR (3 downto 0);
					  bcd_in_1000 : STD_LOGIC_VECTOR (3 downto 0);
					  bcd_in_10000 : STD_LOGIC_VECTOR (3 downto 0);  
					  bcd_in_100000 : STD_LOGIC_VECTOR (3 downto 0);
					  bcd_in_1000000 : STD_LOGIC_VECTOR (3 downto 0)) return std_logic_vector is

			variable temp1,temp2,temp3,temp4,temp5,temp6,temp7 : std_logic_vector(31 downto 0);
			variable dumm,dumm2,dumm3,dumm4,dumm5,dumm6,dumm7,ans : std_logic_vector(35 downto 0);
			variable int0,int1,int2,int3,int4,int5,int6,int7 : INTEGER;
			variable tumm : std_logic_vector(35 downto 0);
			
			begin
			
			temp1:="00000000000000000000000000000001";	-- 1
			temp2:="00000000000000000000000000001010";	-- 10 2
			temp3:="00000000000000000000000001100100";	-- 100 3
			temp4:="00000000000000000000001111101000";	-- 1000 4
			temp5:="00000000000000000010011100010000";	-- 10000 5
			temp6:="00000000000000011000011010100000";	-- 100000 6
			temp7:="00000000000011110100001001000000";	-- 1000000 7

			dumm := unsigned(bcd_in_0) * unsigned(temp1);
			dumm2 := (unsigned(bcd_in_10) * unsigned(temp2)); 
			dumm3 := (unsigned(bcd_in_100) * unsigned(temp3));
			dumm4 := (unsigned(bcd_in_1000) * unsigned(temp4));
			dumm5 := (unsigned(bcd_in_10000) * unsigned(temp5));
			dumm6 := (unsigned(bcd_in_100000) * unsigned(temp6));
			dumm7 := (unsigned(bcd_in_1000000) * unsigned(temp7));
			ans:="000000000000000000000000000000000000";
			tumm := std_logic_vector(dumm2 + dumm + dumm3 + dumm4 + dumm5 + dumm6 + dumm7);
			return tumm(31 downto 0);
		end bcd_2_bin;

--		function decoder (clk: std_logic; Row: std_logic_vector(3 downto 0); Col: std_logic_vector(3 downto 0)) return std_logic_vector is
--		
--			variable DecodeOut: std_logic_vector(4 downto 0);
--			variable sclk: integer:= 0;
--			begin
--				
--				if clk'event and clk = '1' then -- 1ms
--					if sclk = 100000 then --C1 
--						Col:= "0111"; 
--						sclk <= sclk+1; -- check row pins 
--					elsif ((sclk > 110000) AND (sclk < 110010)) then --R1                
--						if Row = "0111" then 
--							DecodeOut <= "01010"; --A                --R2      
--						elsif Row = "1011" then                    
--							DecodeOut <= "01011"; --B                --R3 
--						elsif Row = "1101" then                    
--							DecodeOut <= "01100"; --C                --R4                
--						elsif Row = "1110" then                    
--							DecodeOut <= "01101"; --D                
--						else DecodeOut <= "11111";                
--						
--						end if; sclk <= sclk+1; -- 2ms 
--					elsif sclk = 200000 then --C2 
--						Col<= "1011"; 
--						sclk := sclk+1; -- check row pins 
--					elsif ((sclk > 210000) AND (sclk < 210010)) then            --R1                
--						if Row = "0111" then                     
--							DecodeOut := "00001";    --1                --R2                
--						elsif Row = "1011" then                     
--							DecodeOut := "00100"; --4                --R3                
--						elsif Row = "1101" then                     
--							DecodeOut := "00111"; --7                --R4                
--						elsif Row = "1110" then                     
--							DecodeOut := "00000"; --0                
--						else DecodeOut := "11111";                
--					end if; 
--					sclk <= sclk+1; --3ms 
--					elsif sclk = 300000 then --C3
--						Col<= "1101"; 
--						sclk <= sclk+1; -- check row pins 
--						elsif ((sclk > 310000) AND (sclk < 310010)) then                --R1 
--							if Row = "0111" then                             
--								DecodeOut := "00010"; --2                --R2                
--							elsif Row = "1011" then                     
--								DecodeOut := "00101"; --5                --R3                
--							elsif Row = "1101" then                     
--								DecodeOut := "01000"; --8                --R4                
--							elsif Row = "1110" then                     
--								DecodeOut := "01111"; --F                
--							else DecodeOut := "11111";                
--							end if;     
--							sclk := sclk+1; --4ms 
--						elsif sclk = 400000 then --C4 
--							Col<= "1110"; sclk <= sclk+1; -- check row pins 
--							elsif ((sclk > 410000) AND (sclk < 410010)) then           --R1               
--								if Row = "0111" then                    
--									DecodeOut := "00011"; --3               --R2               
--								elsif Row = "1011" then                    
--									DecodeOut := "00110"; --6               --R3               
--								elsif Row = "1101" then                    
--									DecodeOut := "01001"; --9               --R4               
--								elsif Row = "1110" then                    
--									DecodeOut := "01110"; --E               
--								else DecodeOut := "11111";               
--								end if;                
--								sclk <= sclk + 1;            
--							elsif sclk = 500000 then 
--							sclk <= 0;
--							else sclk <= sclk+1;
--							end if;
--				end if; 
--		
--		
--				return DecodeOut;
--		end decoder;
--		--
		--
--		function bcd_7segment (BCDin : in STD_LOGIC_VECTOR (3 downto 0)) return std_logic_vector is
--			variable Seven_Segment : std_logic_vector(6 downto 0);
--			begin
--			
--			case BCDin is
--				when "0000" =>
--				Seven_Segment := "0000001"; ---0
--				when "0001" =>
--				Seven_Segment := "1001111"; ---1
--				when "0010" =>
--				Seven_Segment := "0010010"; ---2
--				when "0011" =>
--				Seven_Segment := "0000110"; ---3
--				when "0100" =>
--				Seven_Segment := "1001100"; ---4
--				when "0101" =>
--				Seven_Segment := "0100100"; ---5
--				when "0110" =>
--				Seven_Segment := "0100000"; ---6
--				when "0111" =>
--				Seven_Segment := "0001111"; ---7
--				when "1000" =>
--				Seven_Segment := "0000000"; ---8
--				when "1001" =>
--				Seven_Segment := "0000100"; ---9
--				when others =>
--				Seven_Segment := "1111111"; ---null
--			end case;
--			
--			return Seven_Segment;
--		--	
--		end bcd_7segment;

------  Caller function ------------------

		function calling_mod (a : STD_LOGIC_VECTOR (31 downto 0); lca : STD_LOGIC_VECTOR (3 downto 0); rca :  STD_LOGIC_VECTOR (3 downto 0); 
						asin : std_logic; 
						b : STD_LOGIC_VECTOR (31 downto 0);
					  lcb : STD_LOGIC_VECTOR (3 downto 0);
					  rcb : STD_LOGIC_VECTOR (3 downto 0);
					  bsin : std_logic;
					  op: STD_LOGIC_VECTOR (1 downto 0)) return std_logic_vector is
					  variable c: STD_LOGIC_VECTOR(31 downto 0);
					  variable ans: std_logic_vector (32 downto 0);
					  variable sin: std_logic;
					  variable temp,temp1 : std_logic_vector(32 downto 0);
					  variable anss : std_logic_vector(37 downto 0);
					  variable oflow: std_logic;
					  --variable op: STD_LOGIC_VECTOR (1 downto 0);
					  variable fp : std_logic_vector(3 downto 0 );
					  
					  begin
					  oflow:='0';
					  if(op="00") then					-- "+"
							if asin=bsin then
								ans:=adder(a,b,'0');
								c:=ans(31 downto 0);
								sin:=asin;
								oflow:='0';
							else
								if(asin='1') then
									temp:=sub(b,a);
									c:=temp(31 downto 0);
									sin:=temp(32);
									oflow:='0';
								else
									temp:=sub(a,b);
									c:=temp(31 downto 0);
									sin:=temp(32);
									oflow:='0';
								end if;
							end if;
						fp:="0010";
					--if() overflow condition here
					elsif (op="01") then				-- "-"
							if asin=bsin then
								if(asin='1') then
									temp:=sub(b,a);
									c:=temp(31 downto 0); 
									sin:=temp(32);
									oflow:='0';
								else
									temp:=sub(a,b);
									c:=temp(31 downto 0);
									sin:=temp(32);
									oflow:='0';
								end if;
							else 
								if(asin='1') then
									temp:=adder(a,b,'0');
									c:=temp(31 downto 0);
									sin:='1';                -- 1 is negative   0 is positive
									oflow:=temp(32);
								else
									temp:=adder(a,b,'0');
									c:=temp(31 downto 0);
									sin:='0';
									oflow:=temp(32);
							end if;
					end if;
					fp:="0010";
					elsif (op="10") then					-- "*"
						if asin=bsin then
							c:=mult(a,b);
							sin:='0';
						else	
							c:=mult(a,b);
							sin:='1';
						end if;
						fp:="0100";
					else				-- "/"
							if asin=bsin then
								c:=div(a,b);
								sin:='0';
							else
								c:=div(a,b);
								sin:='1';
							end if;
							fp:="1000";
					end if; 
					anss(31 downto 0):=c;
					anss(32):=sin;
					anss(36 downto 33):=fp;
					anss(37):=oflow;
					
					
					return anss;
		end calling_mod;	


		function check_for_overflow(a: std_logic_vector(39 downto 0)) return std_logic is
			variable off:std_logic;
			begin
				if(a(28)='1' or a(29)='1' or a(30)='1' or a(31)='1' or a(32)='1' or a(33)='1') then
					off:='1';
				else
					off:='0';
				end if;
			return off;
		end check_for_overflow;
		
		function check_for_shifting(temps: std_logic_vector(31 downto 0); fp: std_logic_vector(3 downto 0)) return std_logic_vector is
			variable newtemp : std_logic_vector(31 downto 0);
			begin
				newtemp:="00000000000000000000000000000000";
				if(fp="0011") then
					newtemp:=right_shifter(temps,4);
				elsif(fp="0100") then
					newtemp:=right_shifter(temps,8);
				elsif(fp="1000") then
					newtemp:=left_shift32(temps,8);
				else
					newtemp:=temps;
				end if;
			return newtemp;
		end check_for_shifting;
		
signal full_ans : std_logic_vector(37 downto 0);
signal sign : std_logic;
signal fp: std_logic_vector(3 downto 0);
signal bina,binb :  std_logic_vector(31 downto 0);
signal ans_bin,temps,rs: std_logic_Vector(31 downto 0);
signal bintbcd : std_logic_vector(39 downto 0);
signal r1: std_logic_vector(23 downto 0);
begin
          oflow<='0';

			bina <= bcd_2_bin(a(3 downto 0),a(7 downto 4),a(11 downto 8),a(15 downto 12),a(19 downto 16),a(23 downto 20),a(27 downto 24));
			binb <= bcd_2_bin(b(3 downto 0),b(7 downto 4),b(11 downto 8),b(15 downto 12),b(19 downto 16),b(23 downto 20),b(27 downto 24));

			full_ans<=calling_mod(bina,"0000","0000",sin1,binb,"0000","0000",sin2,op);
			ans_bin<=full_ans(31 downto 0);
			sign<=full_ans(32);
			fp <= full_ans(36 downto 33);
			--oflow <= full_ans(37);
			binary_to_bcd:  bin2bcd port map (clk,'0',ans_bin,bintbcd);
			oflow<=check_for_overflow(bintbcd);
			temps<=bintbcd(31 downto 0);
			sinout<=sign;
			--bcdout<=temps;
			rs<=check_for_shifting(temps,fp);
			bcdout<=rs;


end Behavioral;
