		TITL	"1802 EF3 Q Test code"
		; EJCT	60
		CPU		1802

; Listing 2.
; ===============================================
; Revision history:
; 2009-02-09	First version for RCA 1802 based on Z80 CamelForth
;
;
; ===============================================
;
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
; ===============================================
; Execution begins here
START	ORG	$8000
	ldi	$12
	phi 7
	ldi $34
	plo 7
	;sep 1
loop1:
	ldi HIGH latch
	phi 4
	ldi LOW latch
	plo 4
	sex 4
	out IOY
	nop
	out IOY
	nop
	nop
	out IOY
	nop


	ldi HIGH data
	phi 4
	ldi LOW data
	plo 4
	sex 4
	out IOX
	nop
	out IOX
	nop
	out IOX
	nop
	out IOX
	nop
	out IOX
	nop
	out IOX
	nop
	out IOX
	nop
	out IOX
	nop
	br loop1

wait1:
	req
	b3 	wait1
	seq
wait2:
	bn3 wait2
	br wait1
data:
	BYTE $01,$02,$04,$08,$10,$20,$40,$80	
latch:
	BYTE $00,$01,$00
	END