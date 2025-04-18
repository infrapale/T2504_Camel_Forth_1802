		TITL	"1802/1805A Cross-Assembler Test Source File"
		EJCT	60

		CPU	1805

NUMBER		EQU	0

;
; Register Definitions:
;
R0		EQU	0
R1		EQU	1
R2		EQU	2
R3		EQU	3
R4		EQU	4
R5		EQU	5
R6		EQU	6
R7		EQU	7
R8		EQU	8
R9		EQU	9
R10		EQU	10
R11		EQU	11
R12		EQU	12
R13		EQU	13
R14		EQU	14
R15		EQU	15

;
; I/O Port Definitions:
;
P0		EQU  0
P1		EQU	1
P2		EQU	2
P3		EQU	3
P4		EQU	4
P5		EQU	5
P6		EQU	6
P7		EQU	7

;
; The 1805A opcodes in opcode numerical order -- one-byte opcodes first:
;
START		ORG	100H

; 00H - 0FH.
		IDL
		LDN	R1
		LDN	R2
		LDN	R3
		LDN	R4
		LDN	R5
		LDN	R6
		LDN	R7
		LDN	R8
		LDN	R9
		LDN	R10
		LDN	R11
		LDN	R12
		LDN	R13
		LDN	R14
		LDN	R15
; 10H - 1FH.
		INC	R0
		INC	R1
		INC	R2
		INC	R3
		INC	R4
		INC	R5
		INC	R6
		INC	R7
		INC	R8
		INC	R9
		INC	R10
		INC	R11
		INC	R12
		INC	R13
		INC	R14
		INC	R15
; 20H - 2FH.
		DEC	R0
		DEC	R1
		DEC	R2
		DEC	R3
		DEC	R4
		DEC	R5
		DEC	R6
		DEC	R7
		DEC	R8
		DEC	R9
		DEC	R10
		DEC	R11
		DEC	R12
		DEC	R13
		DEC	R14
		DEC	R15
; 30H - 3FH. - BRANCH instructions
;note: A18 expects the argument to the BRANCH instructions
;to have the same high-byte as the page address. So A18 expects
;a 16-bit value, which A18 reduces to the low byte. A18 also checks
;for page-crossing if the address byte is in the next page (see later).
		BR	$  
		BQ	99H	;BRANCH error
		BZ	$
		BDF	$
		B1	$
		B2	$
		B3	$
		B4	$
		NBR	$
		ORG	$-2
		SKP
;note: NBR is specified by RCA as a two-byte instruction. 
;SKP is specified by RCA as a one-byte instruction.
;*BUT* these are the same op-code, 38H. The function is the same,
;to SKIP the byte following the instruction. SKP allows you to
;provide a SKIPped one-byte instruction; NBR does not. 
		BNQ	$
		BNZ	$
		BNF	$
		BN1	$
		BN2	$
		BN3	$
		BN4	$
; 40H - 4FH.
		LDA	R0
		LDA	R1
		LDA	R2
		LDA	R3
		LDA	R4
		LDA	R5
		LDA	R6
		LDA	R7
		LDA	R8
		LDA	R9
		LDA	R10
		LDA	R11
		LDA	R12
		LDA	R13
		LDA	R14
		LDA	R15
; 50H - 5FH.
		STR	R0
		STR	R1
		STR	R2
		STR	R3
		STR	R4
		STR	R5
		STR	R6
		STR	R7
		STR	R8
		STR	R9
		STR	R10
		STR	R11
		STR	R12
		STR	R13
		STR	R14
		STR	R15
; 60H - 6FH.
		IRX
		OUT	P0	;this is an error. 
		OUT	P1
		OUT	P2
		OUT	P3
		OUT	P4
		OUT	P5
		OUT	P6
		OUT	P7
				;we generate an opcode for INP 0 but flag it	
		INP	P0	;68H goes with the two-byte opcodes for the 1804.
		INP	P1
		INP	P2
		INP	P3
		INP	P4
		INP	P5
		INP	P6
		INP	P7
; 70H - 7FH.
		RET
		DIS
		LDXA
		STXD
		ADC
		SDB
		SHRC
		SMB
		SAV
		MARK
		REQ
		SEQ
		ADCI	NUMBER
		SDBI	NUMBER
		SHLC
		SMBI	NUMBER
; 80H - 8FH.
		GLO	R0
		GLO	R1
		GLO	R2
		GLO	R3
		GLO	R4
		GLO	R5
		GLO	R6
		GLO	R7
		GLO	R8
		GLO	R9
		GLO	R10
		GLO	R11
		GLO	R12
		GLO	R13
		GLO	R14
		GLO	R15
; 90H - 9FH.
		GHI	R0
		GHI	R1
		GHI	R2
		GHI	R3
		GHI	R4
		GHI	R5
		GHI	R6
		GHI	R7
		GHI	R8
		GHI	R9
		GHI	R10
		GHI	R11
		GHI	R12
		GHI	R13
		GHI	R14
		GHI	R15
; 0A0H - 0AFH.
		PLO	R0
		PLO	R1
		PLO	R2
		PLO	R3
		PLO	R4
		PLO	R5
		PLO	R6
		PLO	R7
		PLO	R8
		PLO	R9
		PLO	R10
		PLO	R11
		PLO	R12
		PLO	R13
		PLO	R14
		PLO	R15
; 0B0H - 0BFH.
		PHI	R0
		PHI	R1
		PHI	R2
		PHI	R3
		PHI	R4
		PHI	R5
		PHI	R6
		PHI	R7
		PHI	R8
		PHI	R9
		PHI	R10
		PHI	R11
		PHI	R12
		PHI	R13
		PHI	R14
		PHI	R15
; 0C0H - 0CFH.
		LBR	START
		LBQ	START
		LBZ	START
		LBDF	START
		NOP
		LSNQ
		LSNZ
		LSNF
		NLBR	START
		LBNQ	START
		LBNZ	START
		LBNF	START
		LSIE
		LSQ
		LSZ
		LSDF
; 0D0H - 0DFH.
		SEP	R0
		SEP	R1
		SEP	R2
		SEP	R3
		SEP	R4
		SEP	R5
		SEP	R6
		SEP	R7
		SEP	R8
		SEP	R9
		SEP	R10
		SEP	R11
		SEP	R12
		SEP	R13
		SEP	R14
		SEP	R15
; 0E0H - 0EFH.
		SEX	R0
		SEX	R1
		SEX	R2
		SEX	R3
		SEX	R4
		SEX	R5
		SEX	R6
		SEX	R7
		SEX	R8
		SEX	R9
		SEX	R10
		SEX	R11
		SEX	R12
		SEX	R13
		SEX	R14
		SEX	R15
; 0F0H - 0FFH.
		LDX
		OR
		AND
		XOR
		ADD
		SD
		SHR
		SM
		LDI	NUMBER
		ORI	NUMBER
		ANI	NUMBER
		XRI	NUMBER
		ADI	NUMBER
		SDI	NUMBER
		SHL
		SMI	NUMBER

;test page boundary for BRANCH
;A18 flags if the address byte is in the next page. The 1802 BRANCH
;will increment to the next page or BRANCH on condition to the next page. A18 returns 00H
;for an address. Maybe it should return low(address)? This coding is not recommened. - Herb
		PAGE
		ORG	$-1
DDD:		BR	DDD


;
; Now, the unique 1805A opcodes in numerical order by second opcode byte:

; 00H - 0FH
		STPC
		DTC
		SPM2
		SCM2
		SPM1
		SCM1
		LDC
		STM
		GEC
		ETQ
		XIE
		XID
		CIE
		CID
					; 0EH - 0FH are undefined.
; 10H - 1FH
					; 10H - 1FH are undefined.
; 20H - 2FH
		DBNZ	R0, START
		DBNZ	R1, START
		DBNZ	R2, START
		DBNZ	R3, START
		DBNZ	R4, START
		DBNZ	R5, START
		DBNZ	R6, START
		DBNZ	R7, START
		DBNZ	R8, START
		DBNZ	R9, START
		DBNZ	R10, START
		DBNZ	R11, START
		DBNZ	R12, START
		DBNZ	R13, START
		DBNZ	R14, START
		DBNZ	R15, START
; 30H - 3FH
					; 30H - 3DH are undefined.
		BCI	$
		BXI	$
; 40H - 4FH
					; 40H - 4FH are undefined.
; 50H - 5FH
					; 50H - 5FH are undefined.
; 60H - 6FH
		RLXA	R0
		RLXA	R1
		RLXA	R2
		RLXA	R3
		RLXA	R4
		RLXA	R5
		RLXA	R6
		RLXA	R7
		RLXA	R8
		RLXA	R9
		RLXA	R10
		RLXA	R11
		RLXA	R12
		RLXA	R13
		RLXA	R14
		RLXA	R15
; 70H - 7FH
					; 70H - 73H are undefined.
		DADC
					; 75H is undefined.
		DSAV
		DSMB
					; 78H - 7BH are undefined.
		DACI	NUMBER
					; 7DH - 7EH are undefined.
		DSBI	NUMBER
; 80H - 8FH
		SCAL	R0, START
		SCAL	R1, START
		SCAL	R2, START
		SCAL	R3, START
		SCAL	R4, START
		SCAL	R5, START
		SCAL	R6, START
		SCAL	R7, START
		SCAL	R8, START
		SCAL	R9, START
		SCAL	R10, START
		SCAL	R11, START
		SCAL	R12, START
		SCAL	R13, START
		SCAL	R14, START
		SCAL	R15, START
; 90H - 9FH
		SRET	R0
		SRET	R1
		SRET	R2
		SRET	R3
		SRET	R4
		SRET	R5
		SRET	R6
		SRET	R7
		SRET	R8
		SRET	R9
		SRET	R10
		SRET	R11
		SRET	R12
		SRET	R13
		SRET	R14
		SRET	R15
; 0A0H - 0AFH
		RSXD	R0
		RSXD	R1
		RSXD	R2
		RSXD	R3
		RSXD	R4
		RSXD	R5
		RSXD	R6
		RSXD	R7
		RSXD	R8
		RSXD	R9
		RSXD	R10
		RSXD	R11
		RSXD	R12
		RSXD	R13
		RSXD	R14
		RSXD	R15
; 0B0H - 0BFH
		RNX	R0
		RNX	R1
		RNX	R2
		RNX	R3
		RNX	R4
		RNX	R5
		RNX	R6
		RNX	R7
		RNX	R8
		RNX	R9
		RNX	R10
		RNX	R11
		RNX	R12
		RNX	R13
		RNX	R14
		RNX	R15
; 0C0H - 0CFH
		RLDI	R0, START
		RLDI	R1, START
		RLDI	R2, START
		RLDI	R3, START
		RLDI	R4, START
		RLDI	R5, START
		RLDI	R6, START
		RLDI	R7, START
		RLDI	R8, START
		RLDI	R9, START
		RLDI	R10, START
		RLDI	R11, START
		RLDI	R12, START
		RLDI	R13, START
		RLDI	R14, START
		RLDI	R15, START
; 0D0H - 0DFH
					; 0D0H - 0DFH are undefined.
; 0E0H - 0EFH
					; 0E0H - 0EFH are undefined.
; 0F0H - 0FFH
					; 0F0H - 0F3H are undefined.
		DADD
					; 0F5H - 0F6H are undefined.
		DSM
					; 0F8H - 0FBH are undefined.
		DADI	NUMBER
					; 0FDH - 0FEH are undefined.
		DSMI	NUMBER

;
; Let's test the rest of the pseudo-ops while we're at it:
;
		LOAD	R2, START

; loren:  test SCRT "CALL" and "RETN" pseudo ops.		
SCRTST	LDI		000H
		PHI		R0
		RETN			; should give D5, (SEP R5)

		CALL	SCRTST	; should give D4 HI LO, (SEP R4; DW SCRTST; )

; now the "old way".
		SEP		R4		; SCRT CALL
		DW		SCRTST
		SEP		R5		; SCRT EXIT

; FILL
		FILL	$A5, 20		; 20 decimal bytes of 0A5H
		FILL	$0377, 20		; byte values only, error
		FILL	$5A, 512		; only 255 of fill max
		FILL 25			; missing second argument

VARIABLE	SET	-1

		IF	VARIABLE
		BYTE	-1, , +1
		ELSE
		BYTE	+1, , -1
		ENDI

VARIABLE	SET	VARIABLE EQ 0

		IF	VARIABLE
		WORD	, +1
		ELSE
		WORD	, -1
		ENDI

		BLK	10H		;space but no values

		TEXT	"EXPLOSION"

explo:	TEXT	"\x7f\'\"EXPLOSION\n\r\"\'\177"

foo:	TEXT	"FOO\0\n\r"   ;deliberate error \0 not supported


herb1:		db 1,255,"hello there" ;DB, colon, strings
		br $-3		; use of PC reference $
		dw 12EFH, $4321	; DW and alternate base 16
		db %00100100	; base 2
		db $AB		; base 10
		db @377		; base 8
		ds 10		;space but no values

; test nonalphas in labels symbols, a lot gets by ...

alpha:	nop
$beta:	nop
#gamma:	nop
?delta:	nop
@epsilon:	nop
/frank:	nop	;an error
?gamma:	nop
<iota:	nop	;an error
>janus	nop	;an error
!kappa	nop
&lambda	nop
*monty	nop	;an error

		END