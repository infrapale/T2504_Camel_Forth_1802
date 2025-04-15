; LISTING 2.
;
; ===============================================
; CamelForth for the RCA 1802
; Copyright (c) 1994,1995 Bradford J. Rodriguez
; ; Copyright (c) 2009 Harold Rabbie
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

; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
;
; ===============================================
; CAMEL18H.ASM: High Level Words
;   Source code is for the A180 assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================

; SYSTEM VARIABLES & CONSTANTS ==================

;C BL      -- char	an ASCII space
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"BL"
BL:
	sep constpc
	WORD $20

;Z tibsize  -- n	 size of TIB
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"TIBSIZE"
TIBSIZE:
	sep constpc
	WORD 124	  ; 2 chars safety zone

;X tib     -- a-addr	Terminal Input Buffer
	WORD link
	BYTE 0
link      SET $
	BYTE 3,"TIB"
TIB:
	sep constpc
	WORD tibarea

;Z u0      -- a-addr	current user area adrs
;  0 USER U0
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"U0"
U0:
	sep userpc
	WORD 0

;C >IN     -- a-addr	holds offset into TIB
;  2 USER >IN
	WORD link
	BYTE 0
link      SET $
	BYTE 3,">IN"
TOIN:
	sep userpc
	WORD 2

;C BASE    -- a-addr	holds conversion radix
;  4 USER BASE
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"BASE"
BASE:
	sep userpc
	WORD 4

;C STATE   -- a-addr	holds compiler state
;  6 USER STATE
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"STATE"
STATE:
	sep userpc
	WORD 6

;Z dp      -- a-addr	holds dictionary ptr
;  8 USER DP
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"DP"
DP:
	sep userpc
	WORD 8

;Z 'source  -- a-addr	two cells: len, adrs
; 10 USER 'SOURCE
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"\'SOURCE"
TICKSOURCE:
	sep userpc
	WORD 10

;Z latest    -- a-addr	last word in dict.
;   14 USER LATEST
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"LATEST"
LATEST:
	sep userpc
	WORD 14

;Z hp       -- a-addr	HOLD pointer
;   16 USER HP
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"HP"
HP:
	sep userpc
	WORD 16

;Z LP       -- a-addr	Leave-stack pointer
;   18 USER LP
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"LP"
LP:
	sep userpc
	WORD 18

;Z s0       -- a-addr	end of parameter stack
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"S0"
S0:
	sep constpc
	WORD PARAMSTACK

;X PAD       -- a-addr	user PAD buffer
;			= end of hold area!
	WORD link
	BYTE 0
link      SET $
	BYTE 3,"PAD"
PAD:
	sep constpc
	WORD padarea

;Z l0       -- a-addr	bottom of Leave stack
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"L0"
L0:
	sep constpc
	WORD leavestack

;Z r0       -- a-addr	end of return stack
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"R0"
R0:
	sep constpc
	WORD returnstack

;Z uinit    -- addr	initial values for user area
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"UINIT"
UINIT:
	sep varpc
	WORD 0,0,10,0	; reserved,>IN,BASE,STATE
	WORD enddict	; DP
	WORD 0,0		; SOURCE init'd elsewhere
	WORD lastword	; LATEST
	WORD 0		; HP init'd elsewhere

;Z #init    -- n	#bytes of user area init data
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"#INIT"
NINIT:
	sep constpc
	WORD 18

; ARITHMETIC OPERATORS ==========================

;C S>D    n -- d	single -> double prec.
;   DUP 0< ;
	WORD link
	BYTE 0
link      SET $
	BYTE 3,"S>D"
STOD:
	inc psp
	ldn psp		; n hi
	dec psp
	shlc		; sign to carry
	lbdf MINUSONE
	lbr ZERO

;Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;	...a common factor
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"?NEGATE"
QNEGATE:
	inc psp
	lda psp		; n2 hi
	shlc		; sign to carry
	lbdf NEGATE
	sep nextpc

;C ABS     n1 -- +n2	absolute value
;   DUP ?NEGATE ;
	WORD link
	BYTE 0
link      SET $
	BYTE 3,"ABS"
ABS:
	inc psp
	ldn psp		; n1 hi
	dec psp
	shlc		; sign to carry
	lbdf NEGATE
	sep nextpc

;X DNEGATE   d1 -- d2	negate double precision
;   SWAP INVERT SWAP INVERT 1 M+ ;
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"DNEGATE"
DNEGATE:
	sep colonpc
	WORD SWAP,INVERT,SWAP,INVERT,ONE,MPLUS
	WORD EXIT

;Z ?DNEGATE  d1 n -- d2	negate d1 if n negative
;   0< IF DNEGATE THEN ;       ...a common factor
	WORD link
	BYTE 0
link      SET $
	BYTE 8,"?DNEGATE"
QDNEGATE:
	sep colonpc
	WORD ZEROLESS,QBRANCH,DNEG1,DNEGATE
DNEG1:  WORD EXIT

;X DABS     d1 -- +d2	absolute value dbl.prec.
;   DUP ?DNEGATE ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"DABS"
DABS:
	sep colonpc
	WORD DUP,QDNEGATE,EXIT

;C M*     n1 n2 -- d	signed 16*16->32 multiply
;   2DUP XOR >R	carries sign of the result
;   SWAP ABS SWAP ABS UM*
;   R> ?DNEGATE ;
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"M*"
MSTAR:
	sep colonpc
	WORD TWODUP,XORR,TOR
	WORD SWAP,ABS,SWAP,ABS,UMSTAR
	WORD RFROM,QDNEGATE,EXIT

;C SM/REM   d1 n1 -- n2 n3	symmetric signed div
;   2DUP XOR >R			sign of quotient
;   OVER >R			sign of remainder
;   ABS >R DABS R> UM/MOD
;   SWAP R> ?NEGATE
;   SWAP R> ?NEGATE ;
; Ref. dpANS-6 section 3.2.2.1.
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"SM/REM"
SMSLASHREM:
	sep colonpc
	WORD TWODUP,XORR,TOR,OVER,TOR
	WORD ABS,TOR,DABS,RFROM,UMSLASHMOD
	WORD SWAP,RFROM,QNEGATE,SWAP,RFROM,QNEGATE
	WORD EXIT

;C FM/MOD   d1 n1 -- n2 n3	floored signed div'n
;   DUP >R	      divisor 
;   2DUP XOR >R	 sign of quotient 
;   >R		  divisor 
;   DABS R@ ABS UM/MOD 
;   SWAP R> ?NEGATE SWAP	apply sign to remainder 
;   R> 0< IF			if quotient negative, 
;       NEGATE 
;       OVER IF			if remainder nonzero, 
;	R@ ROT - SWAP 1-	adjust rem,quot 
;       THEN 
;   THEN  R> DROP ; 
; Ref. dpANS-6 section 3.2.2.1.
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"FM/MOD"
FMSLASHMOD:
	sep colonpc 
	WORD DUP,TOR 
	WORD TWODUP,XORR,TOR 
	WORD TOR 
	WORD DABS,RFETCH,ABS,UMSLASHMOD 
	WORD SWAP,RFROM,QNEGATE,SWAP 
	WORD RFROM,ZEROLESS,QBRANCH,FMMOD1 
	WORD NEGATE 
	WORD OVER,QBRANCH,FMMOD1 
	WORD RFETCH,ROT,MINUS,SWAP,ONEMINUS 
FMMOD1: 
	WORD RFROM,DROP,EXIT 

;C *      n1 n2 -- n3		signed multiply
;   M* DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 1,"*"
STAR:
	sep colonpc
	WORD MSTAR,DROP,EXIT

;C /MOD   n1 n2 -- n3 n4	signed divide/rem'dr
;   >R S>D R> FM/MOD ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"/MOD"
SLASHMOD:
	sep colonpc
	WORD TOR,STOD,RFROM,FMSLASHMOD,EXIT

;C /      n1 n2 -- n3		signed divide
;   /MOD nip ;
	WORD link
	BYTE 0
link      SET $
	BYTE 1,"/"
SLASH:
	sep colonpc
	WORD SLASHMOD,NIP,EXIT

;C MOD    n1 n2 -- n3		signed remainder
;   /MOD DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 3,"MOD"
MOD_:
	sep colonpc
	WORD SLASHMOD,DROP,EXIT

;C */MOD  n1 n2 n3 -- n4 n5	n1*n2/n3, rem&quot
;   >R M* R> FM/MOD ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"*/MOD"
SSMOD:
	sep colonpc
	WORD TOR,MSTAR,RFROM,FMSLASHMOD,EXIT

;C */     n1 n2 n3 -- n4	n1*n2/n3
;   */MOD nip ;
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"*/"
STARSLASH:
	sep colonpc
	WORD SSMOD,NIP,EXIT

;C MAX    n1 n2 -- n3		signed maximum
;   2DUP < IF SWAP THEN DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 3,"MAX"
MAX:
	sep colonpc
	WORD TWODUP,LESS,QBRANCH,MAX1,SWAP
MAX1:   WORD DROP,EXIT

;C MIN    n1 n2 -- n3		signed minimum
;   2DUP > IF SWAP THEN DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 3,"MIN"
MIN:
	sep colonpc
	WORD TWODUP,GREATER,QBRANCH,MIN1,SWAP
MIN1:   WORD DROP,EXIT

; DOUBLE OPERATORS ==============================

;C 2@    a-addr -- x1 x2	fetch 2 cells
;   DUP CELL+ @ SWAP @ ;
;   the lower address will appear on top of stack
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"2@"
TWOFETCH:
	sep colonpc
	WORD DUP,CELLPLUS,FETCH,SWAP,FETCH,EXIT

;C 2!    x1 x2 a-addr --	store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"2!"
TWOSTORE:
	sep colonpc
	WORD SWAP,OVER,STORE,CELLPLUS,STORE,EXIT

;C 2DROP  x1 x2 --		drop 2 cells
;   DROP DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"2DROP"
TWODROP:
	sep colonpc
	WORD DROP,DROP,EXIT

;C 2DUP   x1 x2 -- x1 x2 x1 x2	dup top 2 cells
;   OVER OVER ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"2DUP"
TWODUP:
	lda psp		; x2 lo
	plo temp2
	lda psp		; x2 hi
	phi temp2
	lda psp		; x1 lo
	plo temp1
	ldn psp		; x1 hi
	dec psp
	dec psp
	dec psp
	dec psp
	stxd
	glo temp1
	stxd
	ghi temp2
	stxd
	glo temp2
	str psp
	sep nextpc

;C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
;   ROT >R ROT R> ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"2SWAP"
TWOSWAP:
	sep colonpc
	WORD ROT,TOR,ROT,RFROM,EXIT

;C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
;   >R >R 2DUP R> R> 2SWAP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"2OVER"
TWOOVER:
	sep colonpc
	WORD TOR,TOR,TWODUP,RFROM,RFROM
	WORD TWOSWAP,EXIT

; INPUT/OUTPUT ==================================

;C COUNT   c-addr1 -- c-addr2 u	counted->adr/len
;   DUP CHAR+ SWAP C@ ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"COUNT"
COUNT:
	sep colonpc
	WORD DUP,CHARPLUS,SWAP,CFETCH,EXIT

;C CR      --			output newline
;   0D EMIT 0A EMIT ;
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"CR"
CR:
	sep colonpc
	WORD LIT,$0D,EMIT,LIT,$0A,EMIT,EXIT

;C SPACE   --			output a space
;   BL EMIT ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"SPACE"
SPACE:
	sep colonpc
	WORD BL,EMIT,EXIT

;C SPACES   n --		output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"SPACES"
SPACES:
	sep colonpc
SPCS1:  WORD DUP,QBRANCH,SPCS2
	WORD SPACE,ONEMINUS,BRANCH,SPCS1
SPCS2:  WORD DROP,EXIT

;Z umin     u1 u2 -- u		unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"UMIN"
UMIN:
	sep colonpc
	WORD TWODUP,UGREATER,QBRANCH,UMIN1,SWAP
UMIN1:  WORD DROP,EXIT

;Z umax    u1 u2 -- u		unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"UMAX"
UMAX:
	sep colonpc
	WORD TWODUP,ULESS,QBRANCH,UMAX1,SWAP
UMAX1:  WORD DROP,EXIT

;C ACCEPT  c-addr +n -- +n'	get line from term'l
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"ACCEPT"
ACCEPT:
	inp 1			; recognized by simulator
	sep nextpc

;C TYPE    c-addr +n --		type line to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"TYPE"
TYPE:
	sep colonpc
	WORD QDUP,QBRANCH,TYP4
	WORD OVER,PLUS,SWAP,XDO
TYP3:   WORD II,CFETCH,EMIT,XLOOP,TYP3
	WORD BRANCH,TYP5
TYP4:   WORD DROP
TYP5:   WORD EXIT

;Z (S")     -- c-addr u		run-time code for S"
;   R> COUNT 2DUP + ALIGNED >R  ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"(S\")"
XSQUOTE:
	sep colonpc
	WORD RFROM,COUNT,TWODUP,PLUS,ALIGNED,TOR
	WORD EXIT

;C S"       --			compile in-line string
;   COMPILE (S")  [ HEX ]
;   22 WORD C@ 1+ ALIGNED ALLOT ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 2,"S\""
SQUOTE:
	sep colonpc
	WORD LIT,XSQUOTE,COMMAXT
	WORD LIT,$22,WORD,CFETCH,ONEPLUS
	WORD ALIGNED,ALLOT,EXIT

;C ."       --			compile string to print
;   POSTPONE S"  POSTPONE TYPE ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 2,".\""
DOTQUOTE:
	sep colonpc
	WORD SQUOTE
	WORD LIT,TYPE,COMMAXT
	WORD EXIT
			
; NUMERIC OUTPUT ================================
; Numeric conversion is done l.s.digit first, so
; the output buffer is built backwards in memory.

; Some double-precision arithmetic operators are
; needed to implement ANSI numeric conversion.

;Z UD/MOD   ud1 u2 -- u3 ud4	32/16->32 divide
;   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"UD/MOD"
UDSLASHMOD:
	sep colonpc
	WORD TOR,ZERO,RFETCH,UMSLASHMOD,ROT,ROT
	WORD RFROM,UMSLASHMOD,ROT,EXIT

;Z UD*      ud1 d2 -- ud3	32*16->32 multiply
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;
	WORD link
	BYTE 0
link      SET $
	BYTE 3,"UD*"
UDSTAR:
	sep colonpc
	WORD DUP,TOR,UMSTAR,DROP
	WORD SWAP,RFROM,UMSTAR,ROT,PLUS,EXIT

;C HOLD  char --		add char to output string
;   -1 HP +!  HP @ C! ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"HOLD"
HOLD:
	sep colonpc
	WORD MINUSONE,HP,PLUSSTORE
	WORD HP,FETCH,CSTORE,EXIT

;C <#    --			 begin numeric conversion
;   PAD HP ! ;			(initialize Hold Pointer)
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"<#"
LESSNUM:
	sep colonpc
	WORD PAD,HP,STORE,EXIT

;Z >digit   n -- c		convert to 0..9A..Z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
	WORD link
	BYTE 0
link      SET $
	BYTE 6,">DIGIT"
TODIGIT:
	sep colonpc
	WORD DUP,LIT,9,GREATER,LIT,7,ANDD,PLUS
	WORD LIT,$30,PLUS,EXIT

;C #     ud1 -- ud2		convert 1 digit of output
;   BASE @ UD/MOD ROT >digit HOLD ;
	WORD link
	BYTE 0
link      SET $
	BYTE 1,"#"
NUM:
	sep colonpc
	WORD BASE,FETCH,UDSLASHMOD,ROT,TODIGIT
	WORD HOLD,EXIT

;C #S    ud1 -- ud2		convert remaining digits
;   BEGIN # 2DUP OR 0= UNTIL ;
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"#S"
NUMS:
	sep colonpc
NUMS1:  WORD NUM,TWODUP,ORR,ZEROEQUAL,QBRANCH,NUMS1
	WORD EXIT

;C #>    ud1 -- c-addr u	end conv., get string
;   2DROP HP @ PAD OVER - ;
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"#>"
NUMGREATER:
	sep colonpc
	WORD TWODROP,HP,FETCH,PAD,OVER,MINUS,EXIT

;C SIGN  n --			add minus sign if n<0
;   0< IF 2D HOLD THEN ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"SIGN"
SIGN:
	sep colonpc
	WORD ZEROLESS,QBRANCH,SIGN1,LIT,$2D,HOLD
SIGN1:  WORD EXIT

;C U.    u --			display u unsigned
;   <# 0 #S #> TYPE SPACE ;
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"U."
UDOT:
	sep colonpc
	WORD LESSNUM,ZERO,NUMS,NUMGREATER,TYPE
	WORD SPACE,EXIT

;C .     n --			display n signed
;   <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
	WORD link
	BYTE 0
link      SET $
	BYTE 1,"."
DOT:
	sep colonpc
	WORD LESSNUM,DUP,ABS,ZERO,NUMS
	WORD ROT,SIGN,NUMGREATER,TYPE,SPACE,EXIT

;C DECIMAL  --			set number base to decimal
;   10 BASE ! ;
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"DECIMAL"
DECIMAL:
	sep colonpc
	WORD LIT,10,BASE,STORE,EXIT

;X HEX     --			set number base to hex
;   16 BASE ! ;
	WORD link
	BYTE 0
link      SET $
	BYTE 3,"HEX"
HEX:
	sep colonpc
	WORD LIT,16,BASE,STORE,EXIT

; DICTIONARY MANAGEMENT =========================

;C HERE    -- addr		returns dictionary ptr
;   DP @ ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"HERE"
HERE:
	sep colonpc
	WORD DP,FETCH,EXIT

;C ALLOT   n --			allocate n bytes in dict
;   DP +! ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"ALLOT"
ALLOT:
	sep colonpc
	WORD DP,PLUSSTORE,EXIT

; Note: , and C, are only valid for combined
; Code and Data spaces.

;C ,    x --			append cell to dict
;   HERE ! 1 CELLS ALLOT ;
	WORD link
	BYTE 0
link      SET $
	BYTE 1,","
COMMA:
	sep colonpc
	WORD HERE,STORE,ONE,CELLS,ALLOT,EXIT

;C C,   char --			append char to dict
;   HERE C! 1 CHARS ALLOT ;
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"C,"
CCOMMA:
	sep colonpc
	WORD HERE,CSTORE,ONE,CHARS,ALLOT,EXIT

; INTERPRETER ===================================
; Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
; are dependent on the structure of the Forth
; header.  This may be common across many CPUs,
; or it may be different.

;C SOURCE   -- adr n	current input buffer
;   'SOURCE 2@ ;	length is at lower adrs
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"SOURCE"
SOURCE:
	sep colonpc
	WORD TICKSOURCE,TWOFETCH,EXIT

;X /STRING  a u n -- a+n u-n	trim string
;   ROT OVER + ROT ROT - ;
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"/STRING"
SLASHSTRING:
	sep colonpc
	WORD ROT,OVER,PLUS,ROT,ROT,MINUS,EXIT

;Z >counted  src n dst --	copy to counted str
;   2DUP C! CHAR+ SWAP CMOVE ;
	WORD link
	BYTE 0
link      SET $
	BYTE 8,">COUNTED"
TOCOUNTED:
	sep colonpc
	WORD TWODUP,CSTORE,CHARPLUS,SWAP,CMOVE,EXIT

;C WORD   char -- c-addr n	word delim'd by char
;   DUP  SOURCE >IN @ /STRING	-- c c adr n
;   DUP >R   ROT SKIP		-- c adr' n'
;   OVER >R  ROT SCAN		-- adr" n"
;   DUP IF CHAR- THEN		SKIP trailing delim.
;   R> R> ROT -   >IN +!	update >IN offset
;   TUCK -			-- adr' N
;   HERE >counted		--
;   HERE			-- a
;   BL OVER COUNT + C! ;	append trailing blank
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"WORD"
WORD:
	sep colonpc
	WORD DUP,SOURCE,TOIN,FETCH,SLASHSTRING
	WORD DUP,TOR,ROT,SKIP
	WORD OVER,TOR,ROT,SCAN
	WORD DUP,QBRANCH,WORD1,ONEMINUS  ; char-
WORD1:  WORD RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
	WORD TUCK,MINUS
	WORD HERE,TOCOUNTED,HERE
	WORD BL,OVER,COUNT,PLUS,CSTORE,EXIT

;Z NFA>LFA   nfa -- lfa		name adr -> link field
;   3 - ;
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"NFA>LFA"
NFATOLFA:
	ldn psp		; lo
	smi $3
	str psp
	inc psp
	ldn psp		; hi
	smbi $0
	stxd
	sep nextpc

;Z NFA>CFA   nfa -- cfa	name adr -> code field
;   COUNT 7F AND + ;	mask off 'smudge' bit
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"NFA>CFA"
NFATOCFA:
	sep colonpc
	WORD COUNT,LIT,$07F,ANDD,PLUS,EXIT

;Z IMMED?    nfa -- f	fetch immediate flag
;   1- C@ ;		nonzero if immed
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"IMMED?"
IMMEDQ:
	sep colonpc
	WORD ONEMINUS,CFETCH,EXIT

;C FIND   c-addr -- c-addr 0	if not found
;C		  xt  1		if immediate
;C		  xt -1		if "normal"
;   LATEST @ BEGIN		-- a nfa
;       2DUP OVER C@ CHAR+	-- a nfa a nfa n+1
;       S=			-- a nfa f
;       DUP IF
;	   DROP
;	   NFA>LFA @ DUP	-- a link link
;       THEN
;   0= UNTIL			-- a nfa  OR  a 0
;   DUP IF
;       NIP DUP NFA>CFA		-- nfa xt
;       SWAP IMMED?		-- xt iflag
;       0= 1 OR			-- xt 1/-1
;   THEN ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"FIND"
FIND:
	sep colonpc
	WORD LATEST,FETCH
FIND1:  WORD TWODUP,OVER,CFETCH,CHARPLUS
	WORD SEQUAL,DUP,QBRANCH,FIND2
	WORD DROP,NFATOLFA,FETCH,DUP
FIND2:  WORD ZEROEQUAL,QBRANCH,FIND1
	WORD DUP,QBRANCH,FIND3
	WORD NIP,DUP,NFATOCFA
	WORD SWAP,IMMEDQ,ZEROEQUAL,ONE,ORR
FIND3:  WORD EXIT

;C LITERAL  x --		append numeric LITeral
;   STATE @ IF ['] LIT ,XT , THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
	WORD link
	BYTE 1
link      SET $
	BYTE 7,"LITERAL"
LITERAL:
	sep colonpc
	WORD STATE,FETCH,QBRANCH,LITER1
	WORD LIT,LIT,COMMAXT,COMMA
LITER1: WORD EXIT

;Z DIGIT?   c -- n -1		if c is a valid digit
;Z	    -- x  0   otherwise
;   [ HEX ] DUP 39 > 100 AND +	silly looking
;   DUP 140 > 107 AND -   30 -	but it works!
;   DUP BASE @ U< ;
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"DIGIT?"
DIGITQ:
	sep colonpc
	WORD DUP,LIT,$39,GREATER,LIT,$100,ANDD,PLUS
	WORD DUP,LIT,$140,GREATER,LIT,$107,ANDD
	WORD MINUS,LIT,$30,MINUS
	WORD DUP,BASE,FETCH,ULESS,EXIT

;Z ?SIGN   adr n -- adr' n' f	get optional sign
;Z  advance adr/n if sign;	return NZ if negative
;   OVER C@			-- adr n c
;   2C - DUP ABS 1 = AND	-- +=-1, -=+1, else 0
;   DUP IF 1+			-- +=0, -=+2
;       >R 1 /STRING R>		-- adr' n' f
;   THEN ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"?SIGN"
QSIGN:
	sep colonpc
	WORD OVER,CFETCH,LIT,$2C,MINUS,DUP,ABS
	WORD ONE,EQUAL,ANDD,DUP,QBRANCH,QSIGN1
	WORD ONEPLUS,TOR,ONE,SLASHSTRING,RFROM
QSIGN1: WORD EXIT

;C >NUMBER  ud adr u -- ud' adr' u'
;C				convert string to number
;   BEGIN
;   DUP WHILE
;       OVER C@ DIGIT?
;       0= IF DROP EXIT THEN
;       >R 2SWAP BASE @ UD*
;       R> M+ 2SWAP
;       1 /STRING
;   REPEAT ;
	WORD link
	BYTE 0
link      SET $
	BYTE 7,">NUMBER"
TONUMBER:
	sep colonpc
TONUM1: WORD DUP,QBRANCH,TONUM3
	WORD OVER,CFETCH,DIGITQ
	WORD ZEROEQUAL,QBRANCH,TONUM2,DROP,EXIT
TONUM2: WORD TOR,TWOSWAP,BASE,FETCH,UDSTAR
	WORD RFROM,MPLUS,TWOSWAP
	WORD ONE,SLASHSTRING,BRANCH,TONUM1
TONUM3: WORD EXIT

;Z ?NUMBER  c-addr -- n -1	string->number
;Z		 -- c-addr 0	if convert error
;   DUP  0 0 ROT COUNT		-- ca ud adr n
;   ?SIGN >R  >NUMBER		-- ca ud adr' n'
;   IF   R> 2DROP 2DROP 0	-- ca 0   (error)
;   ELSE 2DROP NIP R>
;       IF NEGATE THEN  -1	-- n -1   (ok)
;   THEN ;
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"?NUMBER"
QNUMBER:
	sep colonpc
	WORD DUP,ZERO,DUP,ROT,COUNT
	WORD QSIGN,TOR,TONUMBER,QBRANCH,QNUM1
	WORD RFROM,TWODROP,TWODROP,ZERO
	WORD BRANCH,QNUM3
QNUM1:  WORD TWODROP,NIP,RFROM,QBRANCH,QNUM2,NEGATE
QNUM2:  WORD MINUSONE
QNUM3:  WORD EXIT

;Z INTERPRET    i*x c-addr u -- j*x
;Z				interpret given buffer
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
;   'SOURCE 2!  0 >IN !
;   BEGIN
;   BL WORD DUP C@ WHILE	-- textadr
;       FIND			-- a 0/1/-1
;       ?DUP IF			-- xt 1/-1
;	   1+ STATE @ 0= OR	immed or interp?
;	   IF EXECUTE ELSE ,XT THEN
;       ELSE			-- textadr
;	   ?NUMBER
;	   IF POSTPONE LITERAL	converted ok
;	   ELSE COUNT TYPE 3F EMIT CR ABORT  err
;	   THEN
;       THEN
;   REPEAT DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 9,"INTERPRET"
INTERPRET:
	sep colonpc
	WORD TICKSOURCE,TWOSTORE,ZERO,TOIN,STORE
INTER1: WORD BL,WORD,DUP,CFETCH,QBRANCH,INTER9
	WORD FIND,QDUP,QBRANCH,INTER4
	WORD ONEPLUS,STATE,FETCH,ZEROEQUAL,ORR
	WORD QBRANCH,INTER2
	WORD EXECUTE,BRANCH,INTER3
INTER2: WORD COMMAXT
INTER3: WORD BRANCH,INTER8
INTER4: WORD QNUMBER,QBRANCH,INTER5
	WORD LITERAL,BRANCH,INTER6
INTER5: WORD COUNT,TYPE,LIT,$3F,EMIT,CR,ABORT
INTER6:
INTER8: WORD BRANCH,INTER1
INTER9: WORD DROP,EXIT

;C EVALUATE  i*x c-addr u -- j*x	interpret string
;   'SOURCE 2@ >R >R  >IN @ >R
;   INTERPRET
;   R> >IN !  R> R> 'SOURCE 2! ;
	WORD link
	BYTE 0
link      SET $
	BYTE 8,"EVALUATE"
EVALUATE:
	sep colonpc
	WORD TICKSOURCE,TWOFETCH,TOR,TOR
	WORD TOIN,FETCH,TOR,INTERPRET
	WORD RFROM,TOIN,STORE,RFROM,RFROM
	WORD TICKSOURCE,TWOSTORE,EXIT

;C QUIT     --    R: i*x --		interpret from kbd
;   L0 LP !  R0 RP!   0 STATE !
;   BEGIN
;       TIB DUP TIBSIZE ACCEPT  SPACE
;       INTERPRET
;       STATE @ 0= IF CR ." OK" THEN
;   AGAIN ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"QUIT"
QUIT:
	sep colonpc
	WORD L0,LP,STORE
	WORD R0,RPSTORE,ZERO,STATE,STORE
QUIT1:  WORD TIB,DUP,TIBSIZE,ACCEPT,SPACE
	WORD INTERPRET
	WORD STATE,FETCH,ZEROEQUAL,QBRANCH,QUIT2
	WORD CR,XSQUOTE
	BYTE 3,"ok "
	WORD TYPE
QUIT2:  WORD BRANCH,QUIT1

;C ABORT    i*x --   R: j*x --	clear stk & QUIT
;   S0 SP!  QUIT ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"ABORT"
ABORT:
	sep colonpc
	WORD S0,SPSTORE,QUIT	; QUIT never returns

;Z ?ABORT   f c-addr u --	abort & print msg
;   ROT IF TYPE ABORT THEN 2DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"?ABORT"
QABORT:
	sep colonpc
	WORD ROT,QBRANCH,QABO1,TYPE,ABORT
QABO1:  WORD TWODROP,EXIT

;C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;C	 i*x x1 --       R: j*x --      x1<>0
;   POSTPONE S" POSTPONE ?ABORT ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 6,"ABORT\""
ABORTQUOTE:
	sep colonpc
	WORD SQUOTE
	WORD LIT,QABORT,COMMAXT
	WORD EXIT

;C '    -- xt		find word in dictionary
;   BL WORD FIND
;   0= ABORT" ?" ;
	WORD link
	BYTE 0
link      SET $ 
	BYTE 1,"\'"
TICK:   sep colonpc
	WORD BL,WORD,FIND,ZEROEQUAL,XSQUOTE
	BYTE 1,"?"
	WORD QABORT,EXIT

;C CHAR   -- char	parse ASCII character
;   BL WORD 1+ C@ ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"CHAR"
CHAR:
	sep colonpc
	WORD BL,WORD,ONEPLUS,CFETCH,EXIT

;C [CHAR]   --		compile character LITeral
;   CHAR  ['] LIT ,XT  , ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 6,"[CHAR]"
BRACCHAR:
	sep colonpc
	WORD CHAR
	WORD LIT,LIT,COMMAXT
	WORD COMMA,EXIT

;C (    --		SKIP input until )
;   [ HEX ] 29 WORD DROP ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 1,"("
PAREN:
	sep colonpc
	WORD LIT,$29,WORD,DROP,EXIT

; COMPILER ======================================

;Z (CREATE)	-- 	create link, immediate, and name fields
;   LATEST @ , 0 C,	link & immed field
;   HERE LATEST !	new "latest" link
;   BL WORD C@ 1+ ALLOT	name field
	WORD link
	BYTE 0
link      SET $
	BYTE 8,"(CREATE)"
XCREATE:
	sep colonpc
	WORD LATEST,FETCH,COMMA,ZERO,CCOMMA
	WORD HERE,LATEST,STORE
	WORD BL,WORD,CFETCH,ONEPLUS,ALLOT
	WORD EXIT

;C CREATE   --		create an empty definition with 3-byte code field
;   (CREATE) createpc ,CF noop ,XT	code field
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"CREATE"
CREATE:
	sep colonpc
	WORD XCREATE
	WORD LIT,createpc,COMMACF
	WORD LIT,noop,COMMAXT,EXIT	; default DOES> part

;Z SCREATE   --		create an empty definition with 1-byte code field
;   (CREATE) varpc ,CF
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"SCREATE"
SCREATE:
	sep colonpc
	WORD XCREATE
	WORD LIT,varpc,COMMACF,EXIT

;Z (DOES>)  --	run-time action of DOES>
;   R>	      adrs of headless DOES> def'n
;   LATEST @ NFA>CFA 1+ !   code field to fix up
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"(DOES>)"
XDOES:
	sep colonpc
	WORD RFROM,LATEST,FETCH,NFATOCFA,ONEPLUS,STORE
	WORD EXIT

;C DOES>    --		change action of latest def'n
; ANSI 6.1.1250 says that DOES> only applies to CREATE'd 
; definitions, which have a 3-byte CFA
;   COMPILE (DOES>)
;   docolon ,CF ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 5,"DOES>"
DOES:
	sep colonpc
	WORD LIT,XDOES,COMMAXT
	WORD LIT,colonpc,COMMACF,EXIT

;C RECURSE  --		recurse current definition
;   LATEST @ NFA>CFA ,XT ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 7,"RECURSE"
RECURSE:
	sep colonpc
	WORD LATEST,FETCH,NFATOCFA,COMMAXT,EXIT

;C [	--		enter interpretive state
;   0 STATE ! ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 1,"["
LEFTBRACKET:
	sep colonpc
	WORD ZERO,STATE,STORE,EXIT

;C ]	--		enter compiling state
;   -1 STATE ! ;
	WORD link
	BYTE 0
link      SET $
	BYTE 1,"]"
RIGHTBRACKET:
	sep colonpc
	WORD MINUSONE,STATE,STORE,EXIT

;Z HIDE     --		"hide" latest definition
;   LATEST @ DUP C@ 80 OR SWAP C! ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"HIDE"
HIDE:
	sep colonpc
	WORD LATEST,FETCH,DUP,CFETCH,LIT,$80,ORR
	WORD SWAP,CSTORE,EXIT

;Z REVEAL   --		"reveal" latest definition
;   LATEST @ DUP C@ 7F AND SWAP C! ;
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"REVEAL"
REVEAL:
	sep colonpc
	WORD LATEST,FETCH,DUP,CFETCH,LIT,$7F,ANDD
	WORD SWAP,CSTORE,EXIT

;C IMMEDIATE   --	make last def'n immediate
;   1 LATEST @ 1- C! ;	set immediate flag
	WORD link
	BYTE 0
link      SET $
	BYTE 9,"IMMEDIATE"
IMMEDIATE:
	sep colonpc
	WORD ONE,LATEST,FETCH,ONEMINUS,CSTORE
	WORD EXIT

;C :	--		begin a colon definition
;   CREATE HIDE ] colonpc ,CF ;
	WORD link
	BYTE 0
link      SET $
	BYTE 1,":"
COLON:
	sep colonpc
	WORD XCREATE,HIDE,RIGHTBRACKET,LIT,colonpc,COMMACF
	WORD EXIT

;C ;
;   REVEAL  ,EXIT
;   POSTPONE [  ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 1,";"
SEMICOLON:
	sep colonpc
	WORD REVEAL,CEXIT
	WORD LEFTBRACKET,EXIT

;X :NONAME	-- xt	begin a nameless colon definition
; HERE ] colonpc ,CF ;
	WORD link
	BYTE 0
link      SET $
	BYTE 7,":NONAME"
CONONAME:
	sep colonpc
	WORD HERE,RIGHTBRACKET
	WORD LIT,colonpc,COMMACF,EXIT

;C [']  --		find word & compile as LITeral
;   '  ['] LIT ,XT  , ; IMMEDIATE
; When encountered in a colon definition, the
; phrase  ['] xxx  will cause   LIT,xxt  to be
; compiled into the colon definition (where
; (where xxt is the execution token of word xxx).
; When the colon definition executes, xxt will
; be put on the stack.  (All xt's are one cell.)
;    immed BRACTICK,3,['],docolon
	WORD link
	BYTE 1
link      SET $
	BYTE 3,"[\']"     ; tick character
BRACTICK: 
	sep colonpc
	WORD TICK		; get xt of 'xxx'
	WORD LIT,LIT,COMMAXT	; append LIT action
	WORD COMMA,EXIT		; append xt LITeral

;C POSTPONE  --		postpone compile action of word
;   BL WORD FIND
;   DUP 0= ABORT" ?"
;   0< IF   -- xt	non immed: add code to current
;			def'n to compile xt later.
;       ['] LIT ,XT  ,	add "LIT,xt,COMMAXT"
;       ['] ,XT ,XT	to current definition
;   ELSE  ,XT      immed: compile into cur. def'n
;   THEN ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 8,"POSTPONE"
POSTPONE:
	sep colonpc
	WORD BL,WORD,FIND,DUP,ZEROEQUAL,XSQUOTE
	BYTE 1,"?"
	WORD QABORT,ZEROLESS,QBRANCH,POST1
	WORD LIT,LIT,COMMAXT,COMMA
	WORD LIT,COMMAXT,COMMAXT,BRANCH,POST2
POST1:  WORD COMMAXT
POST2:  WORD EXIT
	       
;Z COMPILE   --		append inline execution token
;   R> DUP CELL+ >R @ ,XT ;
; The phrase ['] xxx ,XT appears so often that
; this word was created to combine the actions
; of LIT and ,XT.  It takes an inline LITeral
; execution token and appends it to the dict.
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"COMPILE"
COMPILE:
	sep colonpc
	WORD RFROM,DUP,CELLPLUS,TOR
	WORD FETCH,COMMAXT,EXIT
; N.B.: not used in the current implementation

; CONTROL STRUCTURES ============================

;C IF       -- adrs	conditional forward BRANCH
;   ['] QBRANCH ,BRANCH HERE DUP ,DEST ;
;   IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 2,"IF"
IF:
	sep colonpc
	WORD LIT,QBRANCH,COMMABRANCH
	WORD HERE,DUP,COMMADEST,EXIT

;C THEN     adrs --	resolve forward BRANCH
;   HERE SWAP !DEST ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 4,"THEN"
THEN:
	sep colonpc
	WORD HERE,SWAP,STOREDEST,EXIT

;C ELSE     adrs1 -- adrs2	BRANCH for IF..ELSE
;   ['] BRANCH ,BRANCH  HERE DUP ,DEST
;   SWAP  POSTPONE THEN ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 4,"ELSE"
ELSE:
	sep colonpc
	WORD LIT,BRANCH,COMMABRANCH
	WORD HERE,DUP,COMMADEST
	WORD SWAP,THEN,EXIT

;C BEGIN    -- adrs		target for bwd. BRANCH
;   HERE ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 5,"BEGIN"
BEGIN:
	lbr HERE

;C UNTIL    adrs --		conditional backward BRANCH
;   ['] QBRANCH ,BRANCH  ,DEST ; IMMEDIATE
;   conditional backward BRANCH
	WORD link
	BYTE 1
link      SET $
	BYTE 5,"UNTIL"
UNTIL:
	sep colonpc
	WORD LIT,QBRANCH,COMMABRANCH
	WORD COMMADEST,EXIT

;X AGAIN    adrs --		uncond'l backward BRANCH
;   ['] BRANCH ,BRANCH  ,DEST ; IMMEDIATE
;   unconditional backward BRANCH
	WORD link
	BYTE 1
link      SET $
	BYTE 5,"AGAIN"
AGAIN:
	sep colonpc
	WORD LIT,BRANCH,COMMABRANCH
	WORD COMMADEST,EXIT

;C WHILE    -- adrs		BRANCH for WHILE loop
;   POSTPONE IF SWAP ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 5,"WHILE"
WHILE:
	sep colonpc
	WORD IF,SWAP,EXIT

;C REPEAT   adrs1 adrs2 --	resolve WHILE loop
;   POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 6,"REPEAT"
REPEAT:
	sep colonpc
	WORD AGAIN,THEN,EXIT

;Z >L   x --   L: -- x		move to leave stack
;   CELL LP +!  LP @ ! ;	(L stack grows up)
	WORD link
	BYTE 0
link      SET $
	BYTE 2,">L"
TOL:
	sep colonpc
	WORD CELL,LP,PLUSSTORE,LP,FETCH,STORE,EXIT

;Z L>   -- x   L: x --		move from leave stack
;   LP @ @  CELL NEGATE LP +! ;
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"L>"
LFROM:
	sep colonpc
	WORD LP,FETCH,FETCH
	WORD CELL,NEGATE,LP,PLUSSTORE,EXIT

;C DO       -- adrs   L: -- 0
;   ['] XDO ,XT   HERE		target for bwd BRANCH
;   0 >L ; IMMEDIATE		marker for LEAVEs
	WORD link
	BYTE 1
link      SET $
	BYTE 2,"DO"
DO:
	sep colonpc
	WORD LIT,XDO,COMMAXT,HERE
	WORD ZERO,TOL,EXIT

;Z ENDLOOP   adrs xt --   L: 0 a1 a2 .. aN --
;   ,BRANCH  ,DEST		backward loop
;   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
;				resolve LEAVEs
; This is a common factor of LOOP and +LOOP.
	WORD link
	BYTE 0
link      SET $
	BYTE 7,"ENDLOOP"
ENDLOOP:
	sep colonpc
	WORD COMMABRANCH,COMMADEST
LOOP1:  WORD LFROM,QDUP,QBRANCH,LOOP2
	WORD THEN,BRANCH,LOOP1
LOOP2:  WORD EXIT

;C LOOP    adrs --   L: 0 a1 a2 .. aN --
;   ['] XLOOP ENDLOOP ;  IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 4,"LOOP"
LOOP:
	sep colonpc
	WORD LIT,XLOOP,ENDLOOP,EXIT

;C +LOOP   adrs --   L: 0 a1 a2 .. aN --
;   ['] XPLUSLOOP ENDLOOP ;  IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 5,"+LOOP"
PLUSLOOP:
	sep colonpc
	WORD LIT,XPLUSLOOP,ENDLOOP,EXIT

;C LEAVE    --    L: -- adrs
;   ['] UNLOOP ,XT
;   ['] BRANCH ,BRANCH   HERE DUP ,DEST  >L
;   ; IMMEDIATE      unconditional forward BRANCH
	WORD link
	BYTE 1
link      SET $
	BYTE 5,"LEAVE"
LEAVE:
	sep colonpc
	WORD LIT,UNLOOP,COMMAXT
	WORD LIT,BRANCH,COMMABRANCH
	WORD HERE,DUP,COMMADEST,TOL,EXIT

; OTHER OPERATIONS ==============================

;X WITHIN   n1|u1 n2|u2 n3|u3 -- f	n2<=n1<n3?
;  OVER - >R - R> U< ;			per ANS document
	WORD link
	BYTE 0
link      SET $
	BYTE 6,"WITHIN"
WITHIN:
	sep colonpc
	WORD OVER,MINUS,TOR,MINUS,RFROM,ULESS,EXIT

;C MOVE    addr1 addr2 u --		smart move
;	     VERSION FOR 1 ADDRESS UNIT = 1 CHAR
;  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
;  WITHIN IF  R> CMOVE>	src <= dst < src+n
;       ELSE  R> CMOVE  THEN ;	  otherwise
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"MOVE"
MOVE:
	sep colonpc
	WORD TOR,TWODUP,SWAP,DUP,RFETCH,PLUS
	WORD WITHIN,QBRANCH,MOVE1
	WORD RFROM,CMOVEUP,BRANCH,MOVE2
MOVE1:  WORD RFROM,CMOVE
MOVE2:  WORD EXIT

;C DEPTH    -- +n		number of items on stack
;   SP@ S0 SWAP - 2/ ;		16-BIT VERSION!
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"DEPTH"
DEPTH:
	sep colonpc
	WORD SPFETCH,S0,SWAP,MINUS,TWOSLASH,EXIT

;C ENVIRONMENT?  c-addr u -- false	system query
;		 -- i*x true
;   2DROP 0 ;			the minimal definition!
	WORD link
	BYTE 0
link      SET $
	BYTE 12,"ENVIRONMENT?"
ENVIRONMENTQ:
	sep colonpc
	WORD TWODROP,ZERO,EXIT

; UTILITY WORDS AND STARTUP =====================

;X WORDS    --			list all words in dict.
;   LATEST @ BEGIN
;       DUP COUNT TYPE SPACE
;       NFA>LFA @
;   DUP 0= UNTIL
;   DROP ;
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"WORDS"
WORDS:
	sep colonpc
	WORD LATEST,FETCH
WDS1:   WORD DUP,COUNT,TYPE,SPACE,NFATOLFA,FETCH
	WORD DUP,ZEROEQUAL,QBRANCH,WDS1
	WORD DROP,EXIT

;X .S      --			print stack contents
;   SP@ S0 - IF
;       SP@ S0 2 - DO I @ U. -2 +LOOP
;   THEN ;
	WORD link
	BYTE 0
link      SET $
	BYTE 2,".S"
DOTS:
	sep colonpc
	WORD SPFETCH,S0,MINUS,QBRANCH,DOTS2
	WORD SPFETCH,S0,LIT,2,MINUS,XDO
DOTS1:  WORD II,FETCH
	WORD swapbytes		; parameter stack data is LITtle-endian
	WORD UDOT,LIT,-2,XPLUSLOOP,DOTS1
DOTS2:  WORD EXIT

;X \	--			comment to end of line
; \ 1 WORD DROP ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 1,"\\"
	sep colonpc
	WORD ONE,WORD,DROP,EXIT	

;X .(	--			print to matching right paren
; [ HEX ] 29 WORD COUNT TYPE ; IMMEDIATE
	WORD link
	BYTE 1
link      SET $
	BYTE 2,".("
	sep colonpc
	WORD LIT,$29,WORD,COUNT,TYPE,EXIT	

;Z COLD     --			cold start Forth system
;   UINIT U0 #INIT CMOVE      init user area
;   ." RCA1802 CamelForth etc."
;   ABORT ;
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"COLD"
COLD:
	sep colonpc
	WORD UINIT,U0,NINIT,CMOVE
	WORD XSQUOTE
	BYTE 39			; length of sign-on string
	BYTE "RCA1802 CamelForth v1.03  19 Feb 2009"
	BYTE $0D,$0A
	WORD TYPE,ABORT		; ABORT never returns

; COMMON CONSTANTS =========================

;Z -1	-- -1	
	WORD link
	BYTE 0
link      SET $
	BYTE 2,"-1"
MINUSONE:
	ldi $FF
m1:
	dec psp
	stxd
	str psp
	sep nextpc

;Z 0	-- 0
	WORD link
	BYTE 0
link      SET $
	BYTE 1,"0"
ZERO:
	ldi $00
	br m1

;C FALSE	-- 0	
	WORD link
	BYTE 0
link      SET $
	BYTE 5,"FALSE"
FALSE:
	br ZERO

;C TRUE	-- -1	
	WORD link
	BYTE 0
link      SET $
	BYTE 4,"TRUE"
TRUE:
	br MINUSONE

;Z 1	-- 1	
	WORD link
	BYTE 0
link      SET $
	BYTE 1,"1"
ONE:
	sep constpc
	WORD 1

; EPILOGUE =========================

lastword	EQU link	; nfa of last word in dictionary
enddict		EQU $		; user's code starts here



