		TITL	"1802 IO Test 1"
		; EJCT	60
		CPU		1802

; Listing 2.
; ===============================================
; Revision history:
; 2009-02-09	First version for RCA 1802 based on Z80 CamelForth
;
;
; ===============================================
; https://www.donnelly-house.net/programming/cdp1802/tolkien1802/
; ===============================================
;
; ===============================================

NUMBER		EQU	1234

; Memory map:
PARAMSTACK	EQU	$8000	; top of parameter stack
returnstack	EQU	$9000	; top of return stack
userarea	EQU	$A000	; user area, must be page aligned
tibarea		EQU	$A200	; Terminal Input Buffer
padarea		EQU	$A400	; User Pad Buffer
leavestack	EQU	$B000	; top of leave stack

reset		EQU	$0000 		; cold start, Forth kernel, dictionary
cold		EQU reset

; ===============================================
sepcode		EQU	$D0	; opcode for SEP instruction
; ===============================================
IOX			EQU	2	; inp/out port for IOX instruction
IOY			EQU	1	; inp/out port for IOY instruction
DOUT		EQU 2   ; data out
COUT		EQU 1	; Control Out
; ===============================================
;  Register Ussage
REG_MAINP		EQU 0
REG_DATA_OUT	EQU 3
REG_DATA_INP	EQU 4
REG_L0			EQU 7

; ===============================================
; Outgoing Control Signals
OUT_RDY_BM		EQU		%10000000
INP_ACK_BM		EQU		%01000000
; Incomming Control Signals
OUT_ACK_BM		EQU		%01000000
INP_RDY_BM		EQU		%10000000
CH_MASK_BM		EQU		%00001111

; ===============================================
; Execution begins here
START	ORG	$8000
	LOAD REG_L0, printch

	LOAD REG_DATA_OUT, outbuff
	ldi $55
	str REG_DATA_OUT

loop1:
	LOAD REG_DATA_INP, inpbuff
	LOAD REG_DATA_OUT, outbuff
	sex  REG_DATA_OUT
loop1b:	
	inp DOUT
	; str REG_DATA_OUT
	;out DOUT
	;dec REG_DATA_OUT
	out COUT
	dec REG_DATA_OUT
	br loop1b
	
	
	
	ldi  $55
	str REG_DATA_OUT
	sex	REG_DATA_OUT
loop1a:	
	out COUT
	dec REG_DATA_OUT
	ldn REG_DATA_OUT
	adi $01
	str REG_DATA_OUT
	br loop1a


	sex	REG_DATA_INP
	inp COUT
	str REG_DATA_OUT
	sex	REG_DATA_OUT
	out COUT
	dec REG_DATA_OUT
	br loop1a
	
	
	
	inc REG_DATA_OUT	; +1 Control byte
	ldi %11111111		; +1 OUT_RDY_BM
	str REG_DATA_OUT	; +1
	sex	REG_DATA_OUT
	out COUT			; +2
	
	dec REG_DATA_OUT 	; +1 back to control byte
	dec REG_DATA_OUT 	; +0 back to data byte
	out DOUT			; +1 write data to output
	dec REG_DATA_OUT	; +0
	ldn REG_DATA_OUT	; +0
	adi $01				; +0
	str REG_DATA_OUT	; +0 save byte to be sent at next iteration
	
	inc REG_DATA_OUT	; +1
	;ldi NOT OUT_RDY_BM
	ldi $00				; +1
	str REG_DATA_OUT	; +1
	out COUT			; +2
	dec REG_DATA_OUT 	; +1 back to control byte
wait_ack:
	nop		
	inp COUT			; +1			
	ani OUT_ACK_BM
	bnz wait_ack		; +1
	nop
	br loop1
	
obsolete:	

wait1:
	req
	b3 	wait1
	seq
wait2:
	bn3 wait2
	br wait1

	ORG	$8100
	sep REG_MAINP
printch:	
	out DOUT  	; data
	out DOUT	; Control
	br printch-1
	
; character in D register
	
workmem ORG $8200
outbuff: BLK 4
inpbuff: BLK 4
		
;data:
;	BYTE $01,$02,$04,$08,$10,$20,$40,$80	
;latch:
;	BYTE $00,$01,$00
	END