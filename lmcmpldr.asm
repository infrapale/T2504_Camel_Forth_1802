;
;	BIN LOADER PROGRAM modified by Chuck Yakym Dec 2015
;
;	THE VERSION BELOW HAS BEEN MODIFIED TO LOAD THE 0000H MONITOR BIN FILE INTO RAM MEMORY AT
;		ADDRESS 0000H
;
;	NOTE - I HAD TO INVERT THE EF3 LINE LOGIC TO MATCH MY HARDWARE
;	
;	THIS SOFTWARE HAS BEEN TESTED USING THE FOLLOWING BAUD RATES
;	1200 8N1, 2400 8N1, 4800 8N1 and 9600 8N1 WITHOUT ERROR
;	
;	NOTE- BAUD RATES OF 300 BAUD DOESN'T SEEM TO WORK CORRECTLY
;	THIS MAY BE DUE TO MY INTERFACE BETWEEN MY
;	 LAPTOP AND THE MEMBERSHIP CARD OR MY TERMINAL PROGRAM.
;
;	INVERTED LOGIC - /EF3 when idle is at VCC level
;	OPCODE LISTING OF THIS PROGRAM, (67 BYTES LONG)
;	0000	90 B4 A4 A5 F8 1C B5 E4 72 55 15 84 3A 08 F8 1C
;	0010	B0 3E 11 36 13 F8 00 B2 92 A2 E2 C4 C4 FC 01 3E
;	0020	1D B3 36 22 F8 FF 52 93 3E 28 F6 38 93 FF 01 3A
;	0030	2D 36 34 38 F6 02 76 52 C3 1C 2C 64 93 FF 01 3A
;	0040	3D 30 22

;
;	NORMAL LOGIC - /EF3 when idle is at VSS level
;	OPCODE LISTING OF THIS PROGRAM, (67 BYTES LONG)
;	0000	90 B4 A4 A5 F8 1C B5 E4 72 55 15 84 3A 08 F8 1C
;	0010	B0 36 11 3E 13 F8 00 B2 92 A2 E2 C4 C4 FC 01 36
;	0020	1D B3 3E 22 F8 FF 52 93 36 28 F6 38 93 FF 01 3A
;	0030	2D 3E 34 38 F6 02 76 52 C3 1C 2C 64 93 FF 01 3A
;	0040	3D 30 22
;
;	
;	INSTRUCTIONS ON HOW TO LOAD THE MEMBERSHIP CARD MONITOR PROGRAM VER 1.5
;	MANUALLY LOAD EITHER THE INVERTED LOGIC OR THE NORMAL LOGIC THAT MATCHES YOUR HARDWARE
;	RUN THE SOFTWARE
;	PRESS THE SPACEBAR ON YOUR TERMINAL PROGRAM (REMEMBER TO USE BAUD RATE FROM 1200 TO 4800 ONLY)
;	NOW SEND THE 0000.BIN FILE FROM YOUR TERMINAL PROGRAM TO THE SERIAL PORT.
;	NOTE - 	THE LED'S ON THE FRONT WILL DISPLAY EACH BYTE THAT HAS BEEN TRANSFERRED.
;		WHEN THE 0000.BIN HAS BEEN COMPLETELY LOADED INTO THE MEMBERSHIP CARD
;		THE LEDS ON THE FRON PANEL SHOULD DISPLAY A HEX "0F" (00001111).
;	AFTER THE TRANSFER IS COMPLETE, RESET AND RESTART YOUR MEMBERSHIP CARD
;	NOW HIT THE ENTER KEY FROM YOUR TERMINAL PROGRAM
;	YOU NOW SHOULD BE SEEING THE MEMBERSHIP CARD MONITOR COMMAND PROMPT.
;
;	ENJOY,
;	CHUCK YAKYM
;

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
;	1802 EFx pins are active LOW, so a physically low pin makes EF3 true!
;
;	letters show where we are_______    __numbers tell how many instructions
;	in the waveform	so far		\  /  have been executed since the bit test

	ORG 0000H



;
;	Transfer Memory address 0000h page (256 bytes) from - to Memory address 1C00hex page (256 bytes)
;
;
;	ASSUME R0 IS THE PROGRAM COUNTER AT THIS POINT AND THIS SOFTWARE IS LOADED AT ADDRESS 0000H
;
;Modified by Chuck Yakym 11/27/15
;	Modified the software to transfer itself from address 0000h to address 1C00h so you can use it
;	to transfer the 0000h monitor bin file into the correct address (0000h) in order to run it from 
;	RAM memory
;

TPAGE	EQU	1CH	;MEMORY PAGE THAT THIS PROGRAM IS TRANSFERRED INTO

INIT	GHI 0
	PHI 4
	PLO 4	; R4 = 0000H
	PLO 5
	LDI	TPAGE
	PHI 5	; R5 = 1C00H
	SEX 4	; X = 4
TRANFER	LDXA
	STR 5
	INC 5
	GLO 4
	BNZ	TRANFER	; TRANFER THE WHOLE 1ST 256 BYTE BLOCK TO 1CXX ADDRESS
;
;	NOW TRANSFER CONTROL OVER TO LEE'S IMPROVED SOFTWARE BELOW 
;	NOTICE THAT THE SOFTWARE IS NOW OFFSET TO LOCATION IN PAGE 1CXX HEX

	LDI	TPAGE
	PHI 0



Page	EQU	00H	;example, load at 0000H


			;A. wait for Start bit
LoopA	BN3  LoopA	;   loop while idle (EF3 false, EF3 pin high)
			;    continue when Start bit detected
			;   (EF3 true, EF3 pin goes low)

			;B. wait for Data bit D5=1 to begin
LoopB	B3 LoopB	;   loop while EF3 true (EF3 pin low)
					;   continue when EF3 false (pin goes high)

	LDI Page	;1  set R2 = destination Page address
	PHI 2		;2    high byte = Page XX
	GHI 2		;3    low byte = 00 (and sets D=0)
	PLO 2		;4    (assumes this program is on page 00)
	SEX 2		;5  set X=2 to point to destination
	NOP		;6.5 2 NOPs for 3 instruction times
	NOP		;8   (best detection margin at 9600 baud)

					;   measure remaining time in Data bit D5
Time	ADI 1		;9  D=D+1 for each loop
	BN3  Time	;10 ...loop until end of bit
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



; OK, we're ready to receive bytes

Main	B3 Main	;D. wait for Stop Bit (EF3 pin low)
					;   found it, get ready for next byte
	LDI 0FFH	;     initialize byte to FF (all 1's)
	STR 2		;     and store it in RAM
	GHI 3		;   get Delay


LoopC	BN3  LoopC	;A. wait for Start bit (EF3 pin high)
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

	B3 Zero	;4  test next bit
	SKP		;5   if bit=1 (EF3 pin high), leave DF=1
Zero	SHR		;5   if bit=0 (EF3 pin low), set DF=0
	LDN 2		;6  get byte
	SHRC		;7   shift bit into byte (DF into D7, D0 into DF)
	STR 2		;8   store byte in RAM

; NOTE - I HAD TO MANUALLY TRANSLATE THE INSTRUCTION BELOW C3 XX 2C
;	LBDF NextBit	;9.5 if DF=1, then Start bit hasn't shifted all
					;    the way thru byte. Loop until it does
	DB	0C3H		;LBDF OPCODE
	DB	TPAGE		;POINTS TO MEMORY PAGE THIS PROGRAM IS LOCATED 
	DB	LOW(NextBit)	;LOW ADDRESS BYTE OF NextBit


	OUT 4		;   show byte and INC Pointer


;	ADDED 1 DELAY BETWEEN LAST BIT AND THE STOP BIT
	GHI 3		;1   get Delay constant (skipped on first entry)
Delay1	SMI 1		;2   decrement Delay...
	BNZ Delay1	;3   loop until 0 (leaves D=00, DF=1)

	BR Main		;   get next byte
	END
