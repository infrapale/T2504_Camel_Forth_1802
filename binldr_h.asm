;Improved version by Lee Hart leeahart@earthlink.net 11/1/15
;	counts 8 bits by circulating the Start bit through D until it reaches DF
;	more accurate bit timing
;	should work from 300-9600 baud (with 3.58/2 = 1.79 MHz 1802 clock)

;find baud rate from first byte received: ASCII <space> = 20 hex = 0010 0000 binary

;	serial format is:	idle Start D0 D1 D2 D3 D4 D5 D6 D7 Stop idle
;	so <space> looks like:	  1     0  0  0  0  0  0  1  0  0  1      1
;				        __ __ __ __ __ __    __ __
;	and EF3 pin voltage is:	 ______|  |  |  |  |  |  |__|  |  |__________
;	                               A                 B  C     D
;	1802 EFx pins are active LOW, so a physically low pin makes EF2 true!
;
;	letters show where we are_______    __numbers tell how many instructions
;	in the waveform	so far		\  /  have been executed since the bit test

	ORG 0000H
Page	EQU	01	;example, load at 0100H
			;A. wait for Start bit
LoopA	B3  LoopA	;   loop while idle (EF3 true, EF3 pin low)
			;    continue when Start bit detected
			;   (EF3 false, EF3 pin goes high)

			;B. wait for Data bit D5=1 to begin
LoopB	BN3 LoopB	;   loop while EF3 false (EF3 pin high)
					;   continue when EF3 true (pin goes low)

	LDI Page	;1  set R2 = destination Page address
	PHI 2		;2    high byte = Page XX
	GHI 0		;3    low byte = 00 (and sets D=0)
	PLO 2		;4    (assumes this program is on page 00)
	SEX 2		;5  set X=2 to point to destination

	NOP		;6.5 2 NOPs for 3 instruction times
	NOP		;8   (best detection margin at 9600 baud)

					;   measure remaining time in Data bit D5
Time	ADI 1		;9  D=D+1 for each loop
	B3  Time	;10 ...loop until end of bit
	PHI 3		;C. found end, save Delay in R3.1

; this checks the length of D5 at 10, 12, 14... instruction times
; low baud rates are easy, so let's make it work at 9600 baud
; assume a 3.58/2 or 1.8 MHz 1802 clock speed, where 1 instruction time = 8.94uS
; at 9600 baud, 1 bit = 104.17uS = 11.65 instruction times
; the BN3 at 0003 detects D5 from 0 to 1 instruction time AFTER it starts,
; so D5 appears to end at 10.65 to 11.65 instruction times
;               __                          _____
; earliest D5     |________________________| 
;		_____                          _____
; latest D5          |________________________|
; instruction times  0 1 2 3 4 5 6 7 8 9 10 11 12 13
; BN3 detects D5 .../
; B3 detects end of D5 when Delay=1 .....|      |
; B3 detects end of D5 when Delay=2 ............|
;
; so a 10-instruction loop neatly "windows" the width of D5 to always set
; Delay=2 at 9600 baud. at the other extreme, Delay=182 at 300 baud

	STR 2		; option to display Delay and loop
	OUT 4		; (use to test baud rate auto-detection)
	BR 0

; OK, we're ready to receive bytes

Main	BN3 Main	;D. wait for Stop Bit (EF3 pin low)
					;   found it, get ready for next byte
	LDI 0FFH	;     initialize byte to FF (all 1's)
	STR 2		;     and store it in RAM
	GHI 3		;   get Delay

LoopC	B3  LoopC	;A. wait for Start bit (EF3 pin high)
					;   aha, found it

	SHR		;   Delay/2 to wait 1/2 bit (middle of bit)
	SKP		;   skip to Delay to read next bit
					;   (ideal is 3.5+Delay, this is 3+Delay)
; read next bit

;	there are exactly 9.5 instructions in each loop (if Delay=1)
;	plus 2 more for each increment in Delay (11.5 if Delay=2, 13.5 if Delay=3 etc.)
;	SKP is used to insure identical times for both 0 and 1 bits
;	using LBDF instead of BDF adds 0.5, so we can hit 11.5 instructions/bit
;	with Delay=2 for 9600 baud with an 1802 at 1.8 MHz

NextBit			;   delay one bit-time
	GHI 3		;1   get Delay constant (skipped on first entry)
Delay	SMI 1		;2   decrement Delay...
	BNZ Delay	;3   loop until 0 (leaves D=00, DF=1)

	BN3 Zero	;4  test next bit
	SKP		;5   if bit=1 (EF3 pin low), leave DF=1
Zero	SHR		;5   if bit=0 (EF3 pin high), set DF=0
	LDN 2		;6  get byte
	SHRC		;7   shift bit into byte (DF into D7, D0 into DF)
	STR 2		;8   store byte in RAM
	LBDF NextBit	;9.5 if DF=1, then Start bit hasn't shifted all
					;    the way thru byte. Loop until it does

	OUT 4		;   show byte and INC Pointer
	BR  Main	;   get next byte
	END
