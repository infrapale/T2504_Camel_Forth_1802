		TITL	"1802/1805A Cross-Assembler Test Source File"
		; EJCT	60
		CPU		1802

; Listing 2.
; ===============================================
; CamelForth for the RCA 1802
; Copyright (c) 1994,1995 Bradford J. Rodriguez
; Copyright (c) 2009 Harold Rabbie RCA 1802 port
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
;
;
; Revision history:
; 2009-02-09	First version for RCA 1802 based on Z80 CamelForth
;
;
; ===============================================
; CAMEL18.ASM: Code Primitives
;   Source code is for the A180 assembler.
;   Forth words are documented as follows:
;x   NAME     stack -- stack    description
;   where x=C for ANS Forth Core words, X for ANS
;   Extensions, Z for internal or private words.
;
; Direct-Threaded Forth model for RCA 1802
; 16 bit cell, 8 bit char, 8 bit (byte) adrs unit
;
; ===============================================
;
;	Stack Layouts
;
;	PSP->	TOS.LO		RSP->	RET.HI
;		TOS.HI			RET.LO
;		NEXT.LO
;		NEXT.HI
;
; Both stacks grow from high to low
; Parameter stack is stored LITtle-endian
; Return stack is stored big-endian
; Compiled 16-bit data is stored big-endian
;
; ANSI 3.1.4.1 requires doubles to be stored with the MORE
; significant cell at the top of the stack. Therefore, doubles
; are stored mixed-endian as follows
;
;	PSP->	Byte [2]
;		Byte [3]	; MSB
;		Byte [0]	; LSB
;		Byte [1]
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
; Register usage
codepc		EQU	0	; PC for code words
ip			EQU	1	; Forth interpreter pointer
psp			EQU	2	; Forth parameter stack pointer
rsp			EQU	3	; Forth return stack pointer
nextpc		EQU	4	; PC for Forth inner interpreter
colonpc		EQU	5	; PC for colon definitions
constpc		EQU	6	; PC for CONSTANT definitions
varpc		EQU	7	; PC for VARIABLE and SCREATE definitions
createpc	EQU	8	; PC for CREATE definitions
userpc		EQU	9	; PC for USER definitions

temppc		EQU	15	; temporary registers
temp1		EQU	15
temp2		EQU	14
temp3		EQU	13
temp4		EQU	12

sepcode		EQU	$D0	; opcode for SEP instruction

; ===============================================
; Execution begins here
START	ORG	reset		; cold start address
				; initialize registers
	ldi PARAMSTACK and $00FF
	plo psp
	ldi PARAMSTACK shr 8
	phi psp
	ldi returnstack and $0FF
	plo rsp
	ldi returnstack shr 8
	phi rsp
	ldi nextd and $0FF
	plo nextpc
	ldi nextd shr 8
	phi nextpc
	ldi docolon and $0FF
	plo colonpc
	ldi docolon shr 8
	phi colonpc
	ldi doconst and $0FF
	plo constpc
	ldi doconst shr 8
	phi constpc
	ldi dovar and $0FF
	plo varpc
	ldi dovar shr 8
	phi varpc
	ldi docreate and $0FF
	plo createpc
	ldi docreate shr 8
	phi createpc
	ldi douser and $0FF
	plo userpc
	ldi douser shr 8
	phi userpc

	sex psp		; do arithmetic on param stack
	lbr cold

; INTERPRETER LOGIC =============================
; See also "defining words" at end of this file
;
	sep codepc
nextd:	 		; call with SEP nextpc
	lda ip		;
	phi codepc
	lda ip
	plo codepc
	br nextd - 1

link	SET 0		; end of dictionary

;C EXIT     --      exit a colon definition
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"EXIT"
EXIT:
	lda rsp
	phi ip
	lda rsp
	plo ip
	sep nextpc

;Z LIT      -- x    fetch inline LITeral to stack
; This is the primitive compiled by LITERAL.
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"LIT"
LIT:
	lda ip	; high byte
 	dec psp
	stxd
	lda ip	; low byte
	str psp
	sep nextpc


;C EXECUTE   i*x xt -- j*x   execute Forth word
;C                           at 'xt'
	WORD link
	BYTE 0
link     SET $
	BYTE 7,"EXECUTE"
EXECUTE:
	ldi ex1 and $0FF	; switch to another PC
	plo temppc	; because we need to enter
	ldi ex1 shr 8	; code fields with codepc
	phi temppc
	sep temppc
ex1:
	lda psp		; lo byte
 	plo codepc
	lda psp		; hi byte
	phi codepc
	sep codepc

; DEFINING WORDS ================================

; ENTER, a.k.a. DOCOLON, entered by sep colonpc
; to enter a new high-level thread (colon def'n.)
; (internal code fragment, not a Forth word)

	sep nextpc
docolon:
	glo ip		; push IP to return stack
	dec rsp
	str rsp
	ghi ip		; hi byte
	dec rsp
	str rsp
	ghi codepc	; get PFA
	phi ip
	glo codepc
	plo ip
	br docolon - 1	; reset colonpc

;C VARIABLE   --      define a Forth variable
;   SCREATE 1 CELLS ALLOT ;

	WORD link
	BYTE 0
link     SET $
	BYTE 8,"VARIABLE"
VARIABLE:
	sep colonpc
	WORD SCREATE,ONE,CELLS,ALLOT,EXIT

; DOVAR, code action of VARIABLE, entered by sep varpc

	sep nextpc
dovar:  		; -- a-addr
	ghi codepc	; high byte
	dec psp
	stxd
	glo codepc	; low byte
	str psp
	br dovar - 1	; reset varpc

; DOCREATE, code action of CREATE'd word
	sep codepc
docreate:
	lda codepc		; high byte of DOES> part
	phi temppc
	lda codepc		; low byte of DOES>
	plo temppc

	ghi codepc		; push PFA to param stack
	dec psp
	stxd
	glo codepc
	str psp

	ghi temppc		; need to enter code field
	phi codepc		; with codepc
	glo temppc
	plo codepc
	br docreate - 1	; reset createpc

;C CONSTANT   n --      define a Forth constant
;   (CREATE) constpc ,CF ,
	WORD link
	BYTE 0
link     SET $
	BYTE 8,"CONSTANT"
CONSTANT:
	sep colonpc
	WORD XCREATE,LIT,constpc,COMMACF,COMMA,EXIT

; DOCONST, code action of CONSTANT,
; entered by sep constpc

	sep nextpc
doconst:  ; -- x
	lda codepc		; high byte
	dec psp
	stxd
	lda codepc		; low byte
	str psp
	br doconst - 1	; reset constpc

;Z USER     n --        define user variable 'n'
;   (CREATE) userpc ,CF ,
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"USER"
USER:
	sep colonpc
	WORD XCREATE,LIT,userpc,COMMACF,COMMA,EXIT

; DOUSER, code action of USER,
; entered by sep userpc

	sep nextpc
douser:  ; -- a-addr	; assumes user area is page-aligned
			; and no more than 256 user variables
	ldi userarea shr 8	; address high byte
	dec psp
	stxd
	inc codepc	; point to LSB of user offset
	lda codepc	; ldn codepc is IDL!
	str psp
	br douser - 1	; reset userpc

;C EMIT     c --    output character to console
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"EMIT"
EMIT:
	out 1	; recognized by simulator
	sep nextpc

;Z TRACE   n --		set tracing flags in simulator
	WORD link	; Bit 0 : Forth tracing
	BYTE 0		; Bit 1 : Instruction tracing
link     SET $	; Bit 2 : Echo ACCEPTed line
	BYTE 5,"TRACE"
TRACE:
	out 2	; recognized by simulator
	sep nextpc

;X BYE	i*x --	exit simulation
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"BYE"
BYE:
	ret	; unimplemented instruction

; STACK OPERATIONS ==============================


;C ?DUP     x -- 0 | x x    DUP if nonzero
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"?DUP"
QDUP:
	lda psp		; get low byte
	or		; point to high byte
	dec psp
	bnz DUP
	sep nextpc

;C DUP      x -- x x      duplicate top of stack
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"DUP"
DUP:
	lda psp		; lo byte
	plo temp1
	ldn psp		; high byte
	dec psp
	dec psp
	stxd
	glo temp1
	str psp
	sep  nextpc

;C DROP     x --          drop top of stack
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"DROP"
DROP:
	inc psp
	inc psp
	sep nextpc

;C SWAP     x1 x2 -- x2 x1    swap top two items
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"SWAP"
SWAP:
	lda psp		; x2 lo
	plo temp2
	lda psp		; x2 hi
	phi temp2
	lda psp		; x1 lo
	plo temp1
	ldn psp		; x1 hi
	phi temp1

	ghi temp2
	stxd
 	glo temp2
	stxd
	ghi temp1
	stxd
	glo temp1
	str psp
	sep nextpc

;C OVER    x1 x2 -- x1 x2 x1   per stack diagram
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"OVER"
OVER:
	inc psp
	inc psp
	lda psp		; x1 lo
	plo temp1
	ldn psp		; x1 hi

	dec psp
	dec psp
	dec psp
	dec psp
	stxd
	glo temp1
	str psp
	sep nextpc

;C ROT    x1 x2 x3 -- x2 x3 x1  per stack diagram
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"ROT"
ROT:
	lda psp
	plo temp3
	lda psp
	phi temp3
	lda psp
	plo temp2
	lda psp
	phi temp2
	lda psp
	plo temp1
	ldn psp
	phi temp1

	ghi temp2
	stxd
	glo temp2
	stxd
	ghi temp3
	stxd
	glo temp3
	stxd
	ghi temp1
	stxd
	glo temp1
	str psp
	sep nextpc

;X NIP    x1 x2 -- x2           per stack diagram
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"NIP"
NIP:
	lda psp		; x2 lo
	plo temp2
	lda psp		; x2 hi
	inc psp
	stxd
	glo temp2
	str psp
	sep nextpc

;X TUCK   x1 x2 -- x2 x1 x2     per stack diagram
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"TUCK"
TUCK:
	lda psp		; x2 lo
	plo temp2
	lda psp		; x2 hi
	phi temp2
	lda psp		; x1 lo
	plo temp1
	ldn psp		; x1 hi
	phi temp1

	ghi temp2
	stxd
	glo temp2
	stxd
	ghi temp1
	stxd
	glo temp1
	stxd
	ghi temp2
	stxd
	glo temp2
	str psp
	sep nextpc

;C >R    x --   R: -- x   push to return stack
	WORD link
	BYTE 0
link     SET $
	BYTE 2,">R"
TOR:
	lda psp		; x lo
	dec rsp
	str rsp
	lda psp		; x hi
	dec rsp
	str rsp
	sep nextpc

;C R>    -- x    R: x --   pop from return stack
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"R>"
RFROM:
	lda rsp		; x hi
	dec psp
	stxd
	lda rsp		; x lo
	str psp
	sep nextpc

;C R@    -- x     R: x -- x   fetch from rtn stk
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"R@"
RFETCH:
	lda rsp		; x hi
	dec psp
	stxd
	ldn rsp		; x lo
	str psp
	dec rsp
	sep nextpc

;Z SP@  -- a-addr       get data stack pointer
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"SP@"
SPFETCH:
	glo psp
	plo temp1
	ghi psp
	dec psp
	stxd
	glo temp1
	str psp
	sep nextpc

;Z SP!  a-addr --       set data stack pointer
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"SP!"
SPSTORE:
	lda psp		; a lo
	plo temp1
	ldn psp		; a hi
	phi psp
	glo temp1
	plo psp
	sep nextpc

;Z RP@  -- a-addr       get return stack pointer
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"RP@"
RPFETCH:
	ghi rsp
	dec psp
	stxd
	glo rsp
	str psp
	sep nextpc

;Z RP!  a-addr --       set return stack pointer
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"RP!"
RPSTORE:
	lda psp
	plo rsp
	lda psp
	phi rsp
	sep nextpc

; MEMORY AND I/O OPERATIONS =====================

;C !        x a-addr --   store cell in memory
	WORD link
	BYTE 0
link     SET $
	BYTE 1,"!"
STORE:
	lda psp		; a lo
	plo temp1
	lda psp		; a hi
	phi temp1
	lda psp		; x lo
	plo temp2
	lda psp		; x hi
	str temp1
	inc temp1
	glo temp2
	str temp1	; x lo
	sep nextpc

;C C!      char c-addr --    store char in memory
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"C!"
CSTORE:
	lda psp		; a lo
	plo temp1
	lda psp		; a hi
	phi temp1
	lda psp		; x lo
	str temp1
	inc psp		; toss x hi
	sep nextpc

;C @       a-addr -- x   fetch cell from memory
	WORD link
	BYTE 0
link     SET $
	BYTE 1,"@"
FETCH:
	lda psp		; a lo
	plo temp1
	ldn psp		; a hi
	phi temp1
	lda temp1	; x hi
	stxd
	ldn temp1
	str psp		; x lo
	sep nextpc

;C C@     c-addr -- char   fetch char from memory
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"C@"
CFETCH:
	lda psp		; a lo
	plo temp1
	ldn psp		; a hi
	phi temp1
	ldi 0
	stxd		; zero high byte
	ldn temp1	; c lo
	str psp
	sep nextpc

; ARITHMETIC AND LOGICAL OPERATIONS =============

;C +       n1/u1 n2/u2 -- n3/u3     add n1+n2
	WORD link
	BYTE 0
link     SET $
	BYTE 1,"+"
PLUS:
	lda psp		; n2 lo
	inc psp
	add		; n1 lo
	stxd		; n1+n2 lo
	lda psp		; n2 hi
	inc psp
	adc		; n1 hi
	stxd		; n1+n2 hi
	sep nextpc

;X M+       d n -- d         add single to double
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"M+"
MPLUS:
; Double on stack:  byte[1] byte[0] byte[3] byte[2]

	lda psp		; n lo
	plo temp1
	lda psp		; n hi
	phi temp1

	inc psp
	inc psp		; point to d[0]
	glo temp1
	add
	str psp		; update d[0]
	inc psp		; point to d[1]
	ghi temp1
	adc
	stxd		; update d[1]
	dec psp
	dec psp		; point to d[2]
	ghi temp1	; sign of n
	ani $80
	bnz mp1		; negative ->
	ldi 0		; positive sign extend
	br mp2
mp1:
	ldi $FF	; negative sign extend
mp2:
	phi temp1
	adc
	str psp		; update d[2]
	inc psp		; point to d[3]
	ghi temp1	; get sign extension
	adc
	stxd		; update d[3]
	sep nextpc

;C -      n1/u1 n2/u2 -- n3/u3    subtract n1-n2
	WORD link
	BYTE 0
link     SET $
	BYTE 1,"-"
MINUS:
	lda psp		; n2 lo
	inc psp
	sd		; n1 lo
	stxd		; n1-n2 lo
	lda psp		; n2 hi
	inc psp
	sdb
	stxd		; n1-n2 hi
	sep nextpc

;C AND    x1 x2 -- x3            logical AND
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"AND"
ANDD:
	lda psp		; n2 lo
	inc psp
	and		; n1 lo
	stxd		; n1 and n2 lo
	lda psp		; n2 hi
	inc psp
	and
	stxd		; n1 and n2 hi
	sep nextpc

;C OR     x1 x2 -- x3           logical OR
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"OR"
ORR:
	lda psp		; n2 lo
	inc psp
	or		; n1 lo
	stxd		; n1 | n2 lo
	lda psp		; n2 hi
	inc psp
	or
	stxd		; n1 | n2 hi
	sep nextpc

;C XOR    x1 x2 -- x3            logical XOR
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"XOR"
XORR:
	lda psp		; n2 lo
	inc psp
	xor		; n1 lo
	stxd		; n1 ^ n2 lo
	lda psp		; n2 hi
	inc psp
	xor
	stxd		; n1 ^ n2 hi
	sep nextpc

;C INVERT   x1 -- x2            bitwise inversion
	WORD link
	BYTE 0
link     SET $
	BYTE 6,"INVERT"
INVERT:
	ldn psp		; x lo
	xri $FF
	str psp
	inc psp
	ldn psp		; x hi
	xri $FF
	stxd
	sep nextpc

;C NEGATE   x1 -- x2            two's complement
	WORD link
	BYTE 0
link     SET $
	BYTE 6,"NEGATE"
NEGATE:
	ldn psp		; x1 lo
	sdi $0
	str psp
	inc psp
	ldn psp		; x1 hi
	sdbi $0
	stxd
	sep nextpc

;C 1+      n1/u1 -- n2/u2       add 1 to TOS
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"1+"
ONEPLUS:
	ldn psp		; n1 lo
	adi $1
	str psp
	inc psp
	ldn psp		; n1 hi
	adci $0
	stxd
	sep nextpc

;C 1-      n1/u1 -- n2/u2     subtract 1 from TOS
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"1-"
ONEMINUS:
	ldn psp		; n1 lo
	smi $1
	str psp
	inc psp
	ldn psp		; n1 hi
	smbi $0
	stxd
	sep nextpc

;Z ><      x1 -- x2         swap bytes (not ANSI)
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"><"
swapbytes:
	lda psp
	plo temp1
	ldn psp
	phi temp1
	glo temp1
	stxd
	ghi temp1
	str psp
	sep nextpc

;C 2*      x1 -- x2         arithmetic left shift
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"2*"
TWOSTAR:
	ldn psp		; x lo
	shl		; shift in zero
	str psp
	inc psp
	ldn psp		; x hi
	shlc		; shift in carry
	stxd
	sep nextpc

;C 2/      x1 -- x2        arithmetic right shift
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"2/"
TWOSLASH:		; sign extension
	inc psp
	ldn psp		; x hi
	shlc		; get msb to carry
	ldn psp		; x hi again
	shrc		; shift in carry
	stxd
	ldn psp		; xlo
	shrc
	str psp
	sep nextpc

;C LSHIFT  x1 u -- x2    logical L shift u places
	WORD link
	BYTE 0
link     SET $
	BYTE 6,"LSHIFT"
LSHIFT:
	lda psp		; u lo
	plo temp1
	inc psp		; ignore u hi
lshloop:
	bz shdone

	ldn psp		; lo
	shl		; shift in zero
	str psp
	inc psp
	ldn psp		; hi
	shlc		; shift in carry
	stxd

	dec temp1	; count shifts
	glo temp1
	br lshloop

;C RSHIFT  x1 u -- x2    logical R shift u places
	WORD link
	BYTE 0
link     SET $
	BYTE 6,"RSHIFT"
RSHIFT:
	lda psp		; u lo
	plo temp1
	inc psp		; ignore u hi
rshloop:
	bz shdone

	inc psp
	ldn psp		; hi
	shr		; shift in zero
	stxd
	ldn psp		; lo
	shrc		; shift in carry
	str psp

	dec temp1	; count shifts
	glo temp1
	br rshloop
shdone:
	sep nextpc

;C +!     n/u a-addr --       add cell to memory
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"+!"
PLUSSTORE:
	lda psp		; a lo
	plo temp1
	lda psp		; a hi
	phi temp1	; address
	sex temp1	; do arithmetic in memory

	inc temp1	; low byte
	lda psp		; n lo
	add
	stxd		; low byte
	lda psp		; n hi
	adc
	str temp1	; high byte
	sex psp		; restore
	sep nextpc
	

; COMPARISON OPERATIONS =========================

;C 0=     n/u -- flag    return true if TOS=0
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"0="
ZEROEQUAL:
	lda psp
	bnz xfalse
	ldn psp
	bnz xfalse
xtrue:
	ldi $FF
	stxd
	str psp
	sep nextpc
xfalse:
	ldi $0
	stxd
	str psp
	sep nextpc

;C 0<     n -- flag      true if TOS negative
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"0<"
ZEROLESS:
	inc psp
	ldn psp
	shlc		; sign -> carry
	bdf xtrue
	br xfalse

;C =      x1 x2 -- flag         test x1=x2
	WORD link
	BYTE 0
link     SET $
	BYTE 1,"="
EQUAL:
	lda psp		; low byte x2
	inc psp
	sm		; low byte x1
	inc psp
	bnz xfalse
	dec psp
	dec psp
	lda psp		; high byte x2
	inc psp
	sm
	bnz xfalse
	br xtrue

;X <>     x1 x2 -- flag    test not eq (not ANSI)
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"<>"
NOTEQUAL:
	sep colonpc
	WORD EQUAL,ZEROEQUAL,EXIT

;C <      n1 n2 -- flag        test n1<n2, signed
	WORD link
	BYTE 0
link     SET $
	BYTE 1,"<"
LESS:
	lda psp		; n2 lo
	plo temp2
	lda psp		; n2 hi
	phi temp2
	inc psp		; point to n1 hi
	xor		; compare sign of n1 and n2
	shl
	bdf less2	; different signs ->
	ghi temp2	; n2
less4:
	sm		; n2 - n1 hi
	bz less3	; same, go check lo
	bdf xtrue
	br xfalse
less3:
	dec psp		; point to n1 lo
	glo temp2
	sm
	inc psp		; point to n1 hi
	bz xfalse
	bdf xtrue
	br xfalse
less2:			; here if signs are different
	ghi temp2	; n2 hi
	shl
	bnf xtrue	; positive->
	br xfalse

;C >     n1 n2 -- flag         test n1>n2, signed
	WORD link
	BYTE 0
link     SET $
	BYTE 1,">"
GREATER:
	sep colonpc
	WORD SWAP,LESS,EXIT

;C U<    u1 u2 -- flag       test u1<u2, unsigned
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"U<"
ULESS:
	lda psp		; u2 lo
	plo temp2
	lda psp		; u2 hi
	phi temp2
	inc psp		; point to u1 hi
	br less4

;X U>    u1 u2 -- flag     u1>u2 unsgd (not ANSI)
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"U>"
UGREATER:
	sep colonpc
	WORD SWAP,ULESS,EXIT

; LOOP AND BRANCH OPERATIONS ====================

	PAGE		; avoid out-of-page BRANCHes

;Z BRANCH   --                  BRANCH always
	WORD link
	BYTE 0
link     SET $
	BYTE 6,"BRANCH"
BRANCH:
	lda ip		; dest hi
	phi temp1
	ldn ip		; dest lo
	plo ip
	ghi temp1
	phi ip
	sep nextpc

;Z ?BRANCH   x --              BRANCH if TOS zero
	WORD link
	BYTE 0
link     SET $
	BYTE 7,"?BRANCH"
QBRANCH:
	lda psp		; TOS lo
	or		; TOS hi
	inc psp
	bz BRANCH
	inc ip		; SKIP destination
	inc ip
	sep nextpc

;Z (do)    n1|u1 n2|u2 --  R: -- sys1 sys2
;Z                          run-time code for DO
; '83 and ANSI standard loops terminate when the
; boundary of limit-1 and limit is crossed, in
; either direction.  The RCA1802 doesn't have signed
; overflow logic. (index-limit) is stored on the return
; stack, and the carry bit is used to detect crossing.
; For (+LOOP) with a negative increment, the logic
; is slightly different.
; The limit itself is also stored for use by I.

	WORD link
	BYTE 0
link     SET $
	BYTE 4,"(do)"
XDO:
	lda psp		; index lo
	plo temp1
	lda psp		; index hi
	phi temp1

	lda psp		; limit lo
	dec rsp		; push to return stack
	str rsp		; for use by I
	ldn psp		; limit hi
	dec rsp
	str rsp

	dec psp		; point to limit lo
	glo temp1	; index lo
	sm		; index - limit lo
	dec rsp		
	str rsp		; push to return stack

	inc psp		; point to limit hi
	ghi temp1
	smb		; index - limit hi
	dec rsp		
	str rsp		; push to return stack

	inc psp
	sep nextpc

;Z (loop)   R: sys1 sys2 --  | sys1 sys2
;Z                        run-time code for LOOP
; Add 1 to the loop index.  If loop terminates,
; clean up the return stack and SKIP the BRANCH.
; Else take the inline BRANCH.  Note that LOOP
; terminates when index=0.
	WORD link
	BYTE 0
link     SET $
	BYTE 6,"(loop)"
XLOOP:
	sex rsp		; do arithmetic on return stack
	inc rsp		; low byte of index
	ldi 1
	add		; increment
	stxd
	ldi 0
	adc		; high byte
	str rsp
	sex psp		; restore X
	bnf BRANCH	; no carry, continue loop
	br loopdone

;Z (+loop)   n --   R: sys1 sys2 --  | sys1 sys2
;Z                        run-time code for +LOOP
; Add n to the loop index.  If loop terminates,
; clean up the return stack and SKIP the BRANCH.
; Else take the inline BRANCH.
	WORD link
	BYTE 0
link     SET $
	BYTE 7,"(+loop)"
XPLUSLOOP:
	lda psp		; increment lo
	plo temp1
	lda psp		; increment hi
	phi temp1
	sex rsp		; do arithmetic on return stack
	inc rsp		; lo byte of index'
	glo temp1
	add
	stxd		; update low byte
	ghi temp1
	adc
	str rsp		; update high byte
	sex psp		; restore X
		
	ghi temp1	; 
	ani $80	; sign of increment
	bz XLOOPup	; positive -> 
			; counting down
	bdf BRANCH	; continue looping
	br loopdone

XLOOPup:		; counting up
	bnf BRANCH	; continue looping

loopdone:
	inc ip		; ignore BRANCH destination
	inc ip
	br UNLOOP

;C I        -- n   R: sys1 sys2 -- sys1 sys2
;C                  get the innermost loop index
	WORD link
	BYTE 0
link     SET $
	BYTE 1,"I"
II:
	lda rsp		; index hi
	dec psp		; push to param stack
	stxd
	lda rsp		; index lo
	stxd
	lda rsp		; limit hi
	stxd
	ldn rsp		; limit lo
	str psp

	dec rsp		; restore return stack
	dec rsp
	dec rsp		
	lbr PLUS	; add limit back to index

;C J        -- n   R: 4*sys -- 4*sys
;C                  get the second loop index
	WORD link
	BYTE 0
link     SET $
	BYTE 1,"J"
JJ:
	inc rsp		; SKIP outer loop params
	inc rsp
	inc rsp
	inc rsp

	lda rsp		; index hi
	dec psp		; push to param stack
	stxd
	lda rsp		; index lo
	stxd
	lda rsp		; limit hi
	stxd
	ldn rsp		; limit lo
	str psp

	dec rsp		; restore return stack
	dec rsp
	dec rsp

	dec rsp
	dec rsp	
	dec rsp
	dec rsp	
	lbr PLUS	; add limit back to index

;C UNLOOP   --   R: sys1 sys2 --  drop loop parms
	WORD link
	BYTE 0
link     SET $
	BYTE 6,"UNLOOP"
UNLOOP:
	inc rsp		; drop loop params
	inc rsp		; from return stack
	inc rsp
	inc rsp
	sep nextpc

; MULTIPLY AND DIVIDE ===========================

	PAGE		; avoid out-of-page BRANCHes

;C UM*     u1 u2 -- ud   unsigned 16x16->32 mult.
	WORD link
	BYTE 0
link     SET $
	BYTE 3,"UM*"
UMSTAR:

	lda psp		; u2 lo
	plo temp2
	lda psp		; u2 hi
	phi temp2
	lda psp		; u1 lo
	plo temp1
	ldn psp		; u1 hi
	phi temp1

	ldi 0
	stxd
	stxd
	stxd
	str psp		; clear double result

	plo temp3
	phi temp3	; extend multiplier

; Result on stack:  byte[1] byte[0] byte[3] byte[2]

	ldi 16
	plo temp4	; bit counter
	inc psp
	inc psp
umloop:			; PSP points to byte[0] of result
			
	ghi temp1	; shift u1 right
	shr
	phi temp1
	glo temp1
	shrc
	plo temp1
	bnf um_noadd	; if LSB was 1, add in multiplier

	glo temp2	; byte[0]
	add
	str psp
	inc psp
	ghi temp2	; byte[1]
	adc
	stxd
	dec psp
	dec psp
	glo temp3	; byte[2]
	adc
	str psp
	inc psp
	ghi temp3	; byte[3]
	adc
	str psp		; restore PSP
	inc psp

um_noadd:		; shift multiplier left
	glo temp2
	shl
	plo temp2
	ghi temp2
	shlc
	phi temp2
	glo temp3
	shlc
	plo temp3
	ghi temp3
	shlc
	phi temp3

	dec temp4	; count bits
	glo temp4
	bnz umloop

	dec psp
	dec psp		; point to byte[2]

	sep nextpc

;C UM/MOD   ud u1 -- u2 u3   unsigned 32/16->16
	WORD link
	BYTE 0
link     SET $
	BYTE 6,"UM/MOD"
UMSLASHMOD:
	lda psp		; get divisor u1
	plo temp1
	lda psp
	phi temp1

	ldi 0		; extend divisor to 32 bits
	plo temp2
	phi temp2

	plo temp3	; initialize quotient
	phi temp3

; Dividend on stack:  byte[1] byte[0] byte[3] byte[2]

	ldi 16
	plo temp4	; bit counter
	inc psp

ummodloop:		; PSP points to byte[3] of dividend
			; shift divisor right
	ghi temp1
	shr
	phi temp1
	glo temp1
	shrc
	plo temp1
	ghi temp2
	shrc
	phi temp2
	glo temp2
	shrc
	plo temp2

	ghi temp1	; MSB of divisor
	sd		; dividend - divisor
	bnf umm3	; doesn't go ->
	bnz umd3	; goes ->

	dec psp		; byte[2]
	glo temp1
	sd

	inc psp		; byte[3]
	bnf umm3	; doesn't go ->
	bnz umd3	; goes ->

	inc psp
	inc psp		; byte[1]
	ghi temp2
	sd

	dec psp		; byte[0]
	bnf umm0	; doesn't go ->
	bnz umd0	; goes ->

	glo temp2
	sd
	bnf umm0	; doesn't go ->
	br umd0		; goes ->
umd3:
	inc psp
umd0:

; subtract divisor from dividend
; PSP pointing to byte[0] of dividend
	glo temp2
	sd
	str psp
	inc psp		; byte[1]
	ghi temp2
	sdb
	stxd
	dec psp
	dec psp		; byte[2]
	glo temp1
	sdb
	str psp
	inc psp		; byte[3]
	ghi temp1
	sdb
	str psp
	smi 0		; set carry
	br umm3

umm0:	dec psp

umm3:			; PSP pointing to byte[3] of dividend
			; shift carry into quotient
	glo temp3
	shlc
	plo temp3
	ghi temp3
	shlc
	phi temp3

	dec temp4	; count bits
	glo temp4
	bnz ummodloop

; remainder is byte[0] and byte[1] of the dividend

	ghi temp3	; get msb of quotient
	stxd
	glo temp3	; get lsb of quotient
	str psp
	sep nextpc

; BLOCK AND STRING OPERATIONS ===================

;C FILL   c-addr u char --  fill memory with char
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"FILL"
FILL:
	lda psp
	plo temp1	; char
	inc psp
	lda psp
	plo temp2	; count lo
	lda psp
	phi temp2	; count hi
	lda psp
	plo temp3	; dest lo
	lda psp
	phi temp3	; dest hi

fillloop:
	glo temp2	; check for zero
	bnz fillmore
	ghi temp2
	bz filldone	; done->
fillmore:
	glo temp1
	str temp3	; dst byte
	inc temp3
	dec temp2	; count bytes
	br fillloop
filldone:
	sep nextpc

;X CMOVE   c-addr1 c-addr2 u --  move from bottom
; as defined in the ANSI optional String word set
; On byte machines, CMOVE and CMOVE> are logical
; factors of MOVE.  They are easy to implement on
; CPUs which have a block-move instruction.
	WORD link
	BYTE 0
link     SET $
	BYTE 5,"CMOVE"
CMOVE:
	lda psp
	plo temp1	; count lo
	lda psp
	phi temp1	; count hi
	lda psp
	plo temp2	; dest lo
	lda psp
	phi temp2	; dest hi
	lda psp
	plo temp3	; src lo
	lda psp
	phi temp3	; src hi
cmoveloop:
	glo temp1	; check for zero
	bnz cmovemore
	ghi temp1
	bz cmovedone	; done->
cmovemore:
	lda temp3	; src byte
	str temp2	; dest
	inc temp2
	dec temp1	; count bytes
	br cmoveloop
cmovedone:
	sep nextpc


;X CMOVE>  c-addr1 c-addr2 u --  move from top
; as defined in the ANSI optional String word set
	WORD link
	BYTE 0
link     SET $
	BYTE 6,"CMOVE>"
CMOVEUP:
	sep colonpc
	WORD TOR			; count to return stack
	WORD RFETCH,PLUS		; end of dest + 1
	WORD SWAP,RFETCH,PLUS	; end of src + 1
	WORD RFROM		; count
	WORD xcmoveup,EXIT

xcmoveup:
	lda psp
	plo temp1	; count lo
	lda psp
	phi temp1	; count hi
	lda psp
	plo temp2	; src lo
	lda psp
	phi temp2	; src hi
	dec temp2	; end of src

	lda psp
	plo temp3	; dst lo
	lda psp
	phi temp3	; dst hi
	dec temp3	; end of dst
	sex temp3	; so we can use stxd

xcmoveloop:
	glo temp1	; check for zero
	bnz xcmovemore
	ghi temp1
	bz xcmovedone	; done->
xcmovemore:
	ldn temp2	; src byte
	dec temp2
	stxd		; dest
	dec temp1	; count bytes
	br xcmoveloop
xcmovedone:
	sex psp		; restore X
	sep nextpc

;Z SKIP   c-addr u c -- c-addr' u'
;Z                          SKIP matching chars
; Although SKIP, SCAN, and S= are perhaps not the
; ideal factors of WORD and FIND, they closely
; follow the string operations available on many
; CPUs, and so are easy to implement and fast.
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"SKIP"
SKIP:
	lda psp		; char lo
	plo temp1
	inc psp
	lda psp		; count lo
	plo temp2
	lda psp		; count hi
	phi temp2
	lda psp		; addr lo
	plo temp3
	ldn psp		; addr hi
	phi temp3
	sex temp3	; for comparisons

skloop:			; is count zero?
	glo temp2
	bnz sk1
	ghi temp2
	bz skdone
sk1:
	glo temp1	; get char
	sm
	bnz skdone	; not equal ->
	inc temp3	; increment address
	dec temp2	; decrement count
	br skloop
skdone:
	sex psp		; restore X
	ghi temp3	; push pointer
	stxd
	glo temp3
	stxd
	ghi temp2	; push remaining count
	stxd
	glo temp2
	str psp
	sep nextpc

;Z SCAN    c-addr u c -- c-addr' u'
;Z                      find matching char
	WORD link
	BYTE 0
link     SET $
	BYTE 4,"SCAN"
SCAN:
	lda psp		; char lo
	plo temp1
	inc psp
	lda psp		; count lo
	plo temp2
	lda psp		; count hi
	phi temp2
	lda psp		; addr lo
	plo temp3
	ldn psp		; addr hi
	phi temp3
	sex temp3	; for comparisons

scloop:			; is count zero?
	glo temp2
	bnz sc1
	ghi temp2
	bz skdone
sc1:
	glo temp1	; get char
	sm
	bz skdone	; equal ->
	inc temp3	; increment address
	dec temp2	; decrement count
	br scloop

;Z S=    c-addr1 c-addr2 u -- n   string compare
;Z             n<0: s1<s2, n=0: s1=s2, n>0: s1>s2
	WORD link
	BYTE 0
link     SET $
	BYTE 2,"S="
SEQUAL:
	lda psp		; count lo
	plo temp3
	lda psp		; count hi
	phi temp3
	lda psp		; addr2 lo
	plo temp2
	lda psp		; addr2 hi
	phi temp2
	lda psp		; addr1 lo
	plo temp1
	ldn psp		; addr1 hi
	phi temp1
	sex temp2	; for comparisons

seqloop:
	glo temp3	; is count zero?
	bnz seq1
	ghi temp3
	bz seqdone
seq1:
	lda temp1
	sm		; subtract (addr1) - (addr2)
	bnz seqdone	; not equal ->
	inc temp2
	dec temp3
	br seqloop

seqdone:
	sex psp		; restore X
	stxd		; push result twice
	str psp
	sep nextpc

	INCL "camel18h.asm"
	INCL "camel18d.asm"
	
	END
