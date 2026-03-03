		TITL	"1802 printch Test code"
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
IOX			EQU	4	; inp/out port for IOX instruction
IOY			EQU	1	; inp/out port for IOY instruction
DOUT		EQU 4   ; data out
COUT		EQU 2	; Control Out
; ===============================================
;  Register Ussage
REG_MAINP	EQU 0
REG_DATAP	EQU 3
REG_L0		EQU 7

; ===============================================
; Execution begins here
START	ORG	$8000
	LOAD REG_L0, printch
loop1:
	LOAD REG_DATAP, outbuff
	sex	REG_DATAP
	ldi $55
	str REG_DATAP
	inc REG_DATAP
	ldi $AA
	str REG_DATAP
	dec	REG_DATAP

	sep REG_L0

	LOAD REG_DATAP, outbuff
	ldi $11
	str REG_DATAP
	inc REG_DATAP
	ldi $22
	str REG_DATAP
	dec REG_DATAP
	sep REG_L0
	br 	loop1

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