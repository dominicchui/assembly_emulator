;;; -*- mode: asm -*-
;;; An emulator for an ARM-inspired architecture

	.requ	ir,r14
	.requ	shcount, r4
	.requ	shreg, r4
	.requ	srcreg3, r4
	.requ	exponent, r4	;--- r5 uses ---
	.requ	srcreg2, r5
	.requ	shop, r6
	
	lea	warm,r0		; compute the address of warm program and place in r0
	trap	$SysOverlay	; load warm program: overlay warm program at address

loop:	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer

	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS

	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip
	
equal:
	test	$0b0100,wccr	; EQUAL Z=1
	je	preloop		; if not equal
	mov	typejmp(r0),rip	; interpret instruction

notequal:
	test	$0b0100,wccr	; NOT EQUAL Z=0
	jne	preloop		; if equal
	mov	typejmp(r0),rip	; interpret instruction
	
lessthan:
	mov	wccr, r8	; LESS THAN N!=V
	mov	wccr, r1	
	and	$0b1000,r8
	and	$0b0001,r1
	xor	r8,r1
	je	preloop		;N=V
	mov	typejmp(r0),rip	; interpret instruction
	
lessequal:
	test	$0b0100,wccr	; LESSER OR EQUAL Z=1 or N!=V
	cmovne	typejmp(r0),rip	; if equal

	mov	wccr, r8	; check N!=V
	mov	wccr, r1
	and	$0b1000,r8
	and	$0b0001,r1
	xor	r8,r1
	je	preloop		;N=V and Z=0
	mov	typejmp(r0),rip	; interpret instruction
	
greatequal:
	mov	wccr, r8	; GREATER OR EQUAL  N=V
	mov	wccr, r1	
	and	$0b1000,r8
	and	$0b0001,r1
	xor	r8,r1
	jne	preloop		;N!=V
	mov	typejmp(r0),rip	; interpret instruction

greatthan:	
	test	$0b0100,wccr	; GREATER THAN Z=0 and N=V
	jne	preloop		; if equal

	mov	wccr, r8	; N=V?
	mov	wccr, r1
	and	$0b1000,r8
	and	$0b0001,r1
	xor	r8,r1
 	jne	preloop		;N!=V
	mov	typejmp(r0),rip	; interpret instruction

preloop:
	add	$1,wpc
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer

	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS

	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

halt:	trap	$SysHalt	;halt machine

conjmp:
	.data	halt, preloop, equal, notequal, lessthan, lessequal, greatequal, greatthan ; Decoding table for condition

	
;;; -------------------------INSTRUCTION DECODING---------------------
	
type0:				;--- TYPE 0: DEST & SRC & RHS ---
	mov	r3, r1		;GET DEST -> r1		TYPE 0
	shr	$19, r1
	and	$0b1111, r1

	mov	r3, r8		;GET SRC -> r2
	shr	$15, r8
	and	$0b1111, r8
	mov	wregs(r8),r2

	and	$0x7FFF, r3	;GET RHS -> r3

	mov	r3, r4		;---- DECODE RHS ----
	shr	$12, r4
	mov	rhjmp(r4),rip
	
type1:				;--- TYPE 1: DEST & RHS ---
	mov	r3, r1		;GET DEST -> r1		TYPE 1 (MOV & MVN)
	shr	$19, r1
	and	$0b1111, r1

	and	$0x7FFF, r3	;GET RHS -> r3
	mov	r3,r4		;--- DECODE RHS ---
		
	shr	$12, r4
	mov	rhjmp(r4),rip
	
type2:				;--- TYPE 2: SRC & RHS ---
	mov	r3, r8		;GET SRC -> r2		TYPE 2 (CMP & TST)
	shr	$15, r8
	and	$0b1111, r8
	mov	wregs(r8),r2

	and	$0x7FFF, r3	;GET RHS -> r3
	jne	_reg
	add	$1,wpc
	mov	opjmp(r0),rip	;shortcut if compared to 0
	
_reg:	mov	r3, r4		;--- DECODE RHS ---
	shr	$12, r4
	mov	rhjmp(r4), rip
	
type3:				;--- TYPE 3: RHS --- (ONLY SWI)
	and	$0x7FF, r3	;GET RHS -> r3		TYPE 3 (SWI)
	mov	r3, r4		;--- GET EXPONENT ---
	shr	$9, r4
	
	and	$0x1FF, r3 	;--- GET VALUE ---
	shl	r4, r3		;--- SET VALUE ---
	add	$1,wpc
	add	$1,r15
	
	mov	wr0,r0		;swi
	trap	r3
	mov	r0,wr0
	
	mov	warm(r15),r3	; -------------------- BEGIN MAIN LOOP ---------------------

	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

 	
type31:				;--- TYPE 3-1: RHS --- (ONLY SWIS)
	and	$0x7FF, r3	;GET RHS -> r3		TYPE 3 (SWIS)

	mov	r3, r4		;--- GET EXPONENT ---
	shr	$9, r4

	and	$0x1FF, r3 	;--- GET VALUE ---
	shl	r4, r3		;--- SET VALUE ---
	add	$1,wpc
	add	$1,r15
	
	mov	wr0,r0		;swi
	trap	r3
	mov	r0,wr0
	cmp	$0,r0
	mov	ccr,wccr	

	mov	warm(r15),r3	; -------------------- BEGIN MAIN LOOP ---------------------
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip
	
type4:				;--- TYPE 4: LOAD STORE ---
	mov	r3, r1		;GET DEST -> r1		TYPE 4
	shr	$19, r1
	and	$0b1111, r1

	mov	r3, r8		;GET SRC -> r2
	shr	$15, r8
	and	$0b1111, r8
	mov	wregs(r8),r2

	and	$0x7FFF, r3	;GET RHS -> r3

	mov	r3, r4		;---- DECODE RHS ----
	test	$0x4000,r4	;check bit 14
	jne	_rh2
	add	$1,wpc		; load/store format 1
	shl	$18,r3
	sar	$18,r3
	mov	opjmp(r0),rip

_rh2:	mov	r3, shop	; load/store format 2
	shr	$10, shop
	and	$0b11, shop
	mov	r3, shcount	;--- GET SHIFT COUNT ---	
	and	$0b111111, shcount

	mov	r3, r5		;--- GET SOURCE REGISTER 2 ---
	shr	$6, r5
	and	$0b1111, srcreg2
	mov	wregs(r5),r3

	mov	shopjmp(shop), rip

type5:				;--- TYPE 5: LOAD STORE UPDATE ---
	mov	r3, r1		;GET DEST -> r1		TYPE 5
	shr	$19, r1
	and	$0b1111, r1

	mov	r3, r11		;GET SRC -> r2
	shr	$15, r11
	and	$0b1111, r11
	mov	wregs(r11),r2

	and	$0x7FFF, r3	;GET RHS -> r3

	mov	r3, r4		;---- DECODE RHS ----
	test	$0x4000,r4	;check bit 14
	jne	_rh2
	add	$1,wpc		; load/store format 1
	test	$0x2000,r3	; checks sign of offset
	cmove	opjmp(r0),rip
	add	$0xFFC000,r3	;extend negative number to 24 bits
	mov	opjmp(r0),rip
	
_rh2:	mov	r3, shop	; load/store format 2
	shr	$10, shop
	and	$0b11, shop
	mov	r3, shcount	;--- GET SHIFT COUNT ---	
	and	$0b111111, shcount

	mov	r3, r5		;--- GET SOURCE REGISTER 2 ---
	shr	$6, r5
	and	$0b1111, srcreg2
	mov	wregs(r5),r3

	mov	shopjmp(shop), rip

type6:				;--- TYPE 6: LOAD STORE MULTIPLE --- 
	mov	r3, r1		;GET DEST -> r1		TYPE 6
	shr	$19, r1
	and	$0b1111, r1
	mov	wregs(r1),r2	;r2 holds value in dest reg
	
	and	$0x7FFF, r3	;GET RHS -> r3
	
	mov	r3, r4		;---- DECODE RHS ----
	shr	$12,r4
	mov	rhjmp(r4),rip
	
branch:			
	add	r3,wpc		;--- TYPE BRANCH ---
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

branchlink:
	mov	wpc,r1		
	lea	1(r1),wlr	;--- TYPE BRANCH LINK ---
	add	r3,wpc
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

rh0:	mov	r3,r4		;--- GET EXPONENT ---	RH 0
	shr	$9,r4
	and	$0x1FF, r3	 ;--- GET VALUE ---
	shl	r4, r3		;--- SET VALUE ---
	add	$1,wpc
	mov 	opjmp(r0), rip	;--- CALL METHOD ---

rh4:	mov	r3,shcount	;---GET SHIFT COUNT (r4)---	RH4
	mov	r3,shop		;---GET SHOP (r6)---
	mov	r3, r5		;--- GET SOURCE REGISTER 2 (r5)---
	shr	$6, r5
	and	$0b1111, r5
	mov	wregs(r5),r3
	and	$0b111111, shcount ;---LOOK AT SHIFT COUNT---
	jne	_reg		   ;if shift count !=0
	add	$1,wpc		   ;shortcut
	mov	opjmp(r0), rip
	
_reg:	shr	$10, shop	;use shop
	and	$0b11, shop
	mov	shopjmp(shop),rip
	
rh5:	mov	r3, shop	;--- GET SHOP ---	RH 5
	shr	$10, shop
	and	$0b11, shop

	mov	r3, shreg	;--- GET SHIFT REGISTER ---
	and	$0b1111, shreg

	mov	r3, srcreg2	;--- GET SOURCE REGISTER 2 ---
	shr	$6, srcreg2
	and	$0b1111, srcreg2

	mov	wregs(srcreg2), r3 ;--- SET RHS ---
	mov	wregs(shreg), r4
	
	mov	shopjmp(shop), rip ;--- DECODE SHOP ---
	
rh6:	mov	r3, r4	;-- GET SOURCE REGISTER 3 -> r4 ---	RH 6 (MLA only)
	and	$0b1111, r4
 	 
	shr	$6, r3	; --- GET SOURCE REGISTER 2 -> r3 ---
	and	$0b1111, r3
				
	mov	wregs(r3), r3 ;--- RHS = FUSED MULTIPLY SRCREG3 * SRCREG2 ---
	mul	wregs(r4), r3

	add	$1,wpc	
	
	test    $0b100000,r0	;mlas?
	je	_reg
	add     r3,r2           ;--mla--
	mov     ccr,wccr
	mov     r2,wregs(r1)
	jmp	_done
_reg:	lea	0(r2,r3),wregs(r1) 
_done:	mov     wpc,r15	; -------------------- BEGIN MAIN LOOP ---------------------
	and     $0xFFFFFF,r15
	mov     warm(r15),r3 ; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov     r3,r9	; isolate condition
	shr     $29,r9
        mov     conjmp(r9),rip
	
shop0:	shl	r4, r3		;SHL
	add	$1,wpc
	mov	opjmp(r0), rip	;--- CALL METHOD ---

shop1:	shr	r4, r3		;SHR
	add	$1,wpc	
	mov	opjmp(r0), rip	;--- CALL METHOD ---
	
shop2:	sar	r4, r3		;SAR
	add	$1,wpc	
	mov	opjmp(r0), rip	;--- CALL METHOD ---

shop3:	mov 	r3, r5		   ;ROR
	mov	$1, r10		   ;--- Make Carried-Out-Bits Bitmask ---
	shl	shcount, r10
	sub	$1, r10		;--- Mask Created -> r10
	
	and	r10, r5		;Save Carry-Out Bits -> r5 ---
	
	shr	shcount, r3	   ;--- Shift r3 right by shcount bits ---

	mov	$32, r9		;--- Mask Carried-out bits back in ---
	sub	shcount, r9	;--- Shift left carried-out bits by 32 - shcount ---
	shl	r9, r5
	or	r5, r3		; --- OR ROTATED BITS BACK IN

	add	$1,wpc	
	mov	opjmp(r0), rip	;--- CALL METHOD ---	
	
shopjmp:
	.data	shop0, shop1, shop2, shop3 ; Decoding table for SHOP

rhjmp:
	.data	rh0, rh0, rh0, rh0, rh4, rh5, rh6 ; Decoding table for RHS
	
typejmp:
	.data	type0,type0,type0,type2,type0,type0,type0,type2 ;type 0: requires both dest & src
	.data	type0,type0,type0,type1,type1,type3,type6,type6	;type 1: requires only dest
	.data	type4,type4,type5,type5,type4,halt,halt,halt	        ;type 2: requires only src
	.data	branch,branch,branchlink,branchlink,halt,halt,halt,halt		;type 3: requires neither dest or src
	.data	type0,type0,type0,type2,type0,type0,type0,type2 ;this half for s=1
	.data	type0,type0,type0,type1,type1,type31,type6,type6	;type 4: load/store
	.data	type4,type4,type5,type5,type4,halt,halt,halt		;type 5: load/store update
	.data	branch,branch,branchlink,branchlink ;type 6: load/store multiple


;;; -------------------OPCODE DECODING---------------------
	
;;; dest in r1, lhs in r2, rhs in r3
	
wadd:	lea	0(r2,r3),wregs(r1) ;-add--
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wadc:	test	$0b100000,r0	;adcs?
	je	_reg
	add	r3,r2
	mov	ccr,r4		;ccr1 in r4
	and	$0b11,r4	;save C and V
	mov	wccr,r8		
	and	$0b10,r8
	shr	$1,r8
	add	r8,r2
	mov	ccr,wccr	;ccr2 in wccr (NZ stand, C and V orr)
	or	r4,wccr
	mov	r2,wregs(r1)
	jmp	reloop
	
_reg:	test	$0b10,wccr	;--adc-- check carry bit (NZCV)
	je	_no		;not set
	lea	1(r2,r3),wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

_no:	lea	0(r2,r3),wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip
	
wsub:	sub     r3,r2           ;--sub--
	mov     r2,wregs(r1)
	mov     wpc,r15	; -------------------- BEGIN MAIN LOOP ---------------------
	and     $0xFFFFFF,r15
	mov     warm(r15),r3 ; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov     r3,r9	; isolate condition
	shr     $29,r9
	mov     conjmp(r9),rip

wcmp:	sub	r3,r2		;--cmp--
	mov	ccr,wccr
	add	$1,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

weor:	test	$0b100000,r0
	je	_reg
	xor	r3,r2
	mov	ccr,wccr
	jmp	_done
_reg:	xor	r3,r2		;--eor--
_done:	mov	r2,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

worr:	test	$0b100000,r0
	je	_reg
	or	r3,r2
	mov	ccr,wccr
	jmp	_done
_reg:	or	r3,r2		;--orr--
_done:	mov	r2,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wand:	and	r3,r2		;--and--
	mov	r2,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wtst:	test	r3,r2		;--tst--
	mov	ccr,wccr	;set ccr
	add	$1,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wmul:	test	$0b100000,r0
	je	_reg
	mul	r3,r2		;--mul--
	mov	ccr,wccr
	jmp	_done
_reg:	mul	r3,r2
_done:	mov	r2,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wdiv:	test	$0b100000,r0
	je	_reg
	div	r3,r2		;--div--
	mov	ccr,wccr
	jmp	_done
_reg:	div	r3,r2
_done:	mov	r2,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wmov:	mov	r3,wregs(r1)	;--mov--
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wmvn:	xor	$0xFFFFFFFF,r3	;--mvn--
	mov	r3,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wldr:				;dest in r1, base value in r2, disp in r3, base reg in r11
	add	r3,r2		;--ldr-- forms warm address from base and disp
	and	$0xFFFFFF,r2	;ensure address is within address space
	mov	warm(r2),wregs(r1)
	test	$0b100000,r0	;ldrs?
	je	reloop
	cmp	$0,warm(r2)
	mov	ccr,wccr
	jmp	reloop
	
wstr:	add	r3,r2	        ;--str--
	and	$0xFFFFFF,r2	;ensure address is within address space
	mov	wregs(r1),warm(r2)	;move to warm address
	test	$0b100000,r0	;strs?
	je	reloop
	cmp	$0,wregs(r1)
	mov	ccr,wccr
	mov	conjmp(r9),rip
	jmp	reloop
	
wldu:	test	$0x800000,r3	;check if negative
	jne	_neg		;--ldu-
 	and	$0xFFFFFF,r2	;mask
	mov	warm(r2),wregs(r1)
	test	$0b100000,r0	;ldus?
	je	_donep
	cmp	$0,warm(r2)
	mov	ccr,wccr

_donep:	add	r2,r3		;base+offset in r3
	mov	r3,wregs(r11)	;write address to base
	jmp	reloop
		
_neg:	add	r2,r3		;--ldu neg-- effective address in r3
	and	$0xFFFFFF,r3
	mov	warm(r3),wregs(r1)
	test	$0b100000,r0	;ldus?
	je	_donen
	cmp	$0,warm(r3)
	mov	ccr,wccr

_donen:	mov	r3,wregs(r11)	;write address to base register
	jmp	reloop
	
wstu:	test	$0x800000,r3	;check if negative
	jne	_neg		;--stu--
 	and	$0xFFFFFF,r2	;mask
	mov	wregs(r1),warm(r2)	
	test	$0b100000,r0	;stus?
	je	_donep
	cmp	$0,wregs(r1)
	mov	ccr,wccr
	
_donep:	add	r2,r3		;base+offset in r3
	mov	r3,wregs(r11)
	jmp	reloop
	
_neg:	add	r2,r3		;--stu neg--  effective address in r3
	and	$0xFFFFFF,r3
	mov	wregs(r1),warm(r3)
	test	$0b100000,r0	;stus?
	je	_donen
	cmp	$0,wregs(r1)
	mov	ccr,wccr 

_donen:	mov	r3,wregs(r11)	;write address to base register
	
reloop:	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip
	
wadr:	lea	0(r2,r3),wregs(r1) ;--adr--
	and	$0xFFFFFF,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip
	
wldm:	and	$0xFFFF,r3	;--ldm-- r1 is destination register, r2 is value of dest reg, r3 holds mask
	and	$0xFFFFFF,r2	;ensure r2 is within 24 bit address space
	mov	r3,r4
	and	$1,r4
	je	_1
	mov	warm(r2),wr0	;load wr0
	add	$1,r2
_1:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_2
	mov	warm(r2),wr1	;load wr1
	add	$1,r2
_2:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_3
	mov	warm(r2),wr2	;load wr2
	add	$1,r2
_3:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_4
	mov	warm(r2),wr3	;load wr3
	add	$1,r2
_4:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_5
	mov	warm(r2),wr4	;load wr4
	add	$1,r2
_5:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_6
	mov	warm(r2),wr5	;load wr5
	add	$1,r2
_6:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_7
	mov	warm(r2),wr6	;load wr6
	add	$1,r2
_7:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_8
	mov	warm(r2),wr7	;load wr7
	add	$1,r2
_8:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_9
	mov	warm(r2),wr8	;load wr8
	add	$1,r2
_9:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_10
	mov	warm(r2),wr9	;load wr9
	add	$1,r2
_10:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_11
	mov	warm(r2),wr10	;load wr10
	add	$1,r2
_11:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_12
	mov	warm(r2),wr11	;load wr11
	add	$1,r2
_12:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_13
	mov	warm(r2),wr12	;load wr12
	add	$1,r2
_13:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_14
	mov	warm(r2),wr13	;load wr13
	add	$1,r2
_14:	shr	$1,r3
	je	donem
	mov	r3,r4
	and	$1,r4
	je	_15
	mov	warm(r2),wr14	;load wr14
	add	$1,r2
_15:	shr	$1,r3
	je	donem
	mov	r3,r4
	mov	warm(r2),wr15	;load wr15
	add	$1,r2
	test	$0b100000,r0	;"s"?
	je	donem
	mov	wr15,wccr	;save wccr
	shr	$28,wccr
	jmp	donem
	
wstm:	and	$0xFFFF,r3	;--stm-- r1 is destination register, r2 is value of dest reg, r3 holds mask
	and	$0xFFFFFF,r2	;ensure r2 address is within address space
	test	$0x8000,r3
	je	_14
	sub	$1,r2
	mov	wccr,r8		;save ccr onto pc
	shl	$28,r8
	and	$0xFFFFFF,wpc
	or	r8,wr15
	mov	wr15,warm(r2)	;store wr15
_14:	test	$0x4000,r3
	je	_13
	sub	$1,r2
	mov	wr14,warm(r2)	;store wr14
_13:	test	$0x2000,r3
	je	_12
	sub	$1,r2
	mov	wr13,warm(r2)	;store wr13
_12:	test	$0x1000,r3
	je	_11
	sub	$1,r2
	mov	wr12,warm(r2)	;store wr12
_11:	test	$2048,r3
	je	_10
	sub	$1,r2
	mov	wr11,warm(r2)	;store wr11
_10:	test	$1024,r3
	je	_9
	sub	$1,r2	
	mov	wr10,warm(r2)	;store wr10
_9:	test	$512,r3
	je	_8
	sub	$1,r2	
	mov	wr9,warm(r2)	;store wr9
_8:	test	$256,r3
	je	_7
	sub	$1,r2
	mov	wr8,warm(r2)	;store wr8
_7:	test	$128,r3
	je	_6
	sub	$1,r2
	mov	wr7,warm(r2)	;store wr7
_6:	test	$64,r3
	je	_5
	sub	$1,r2	
	mov	wr6,warm(r2)	;store wr6
_5:	test	$32,r3
	je	_4
	sub	$1,r2
	mov	wr5,warm(r2)	;store wr5
_4:	test	$16,r3
	je	_3
	sub	$1,r2
	mov	wr4,warm(r2)	;store wr4
_3:	test	$8,r3
	je	_2
	sub	$1,r2	
	mov	wr3,warm(r2)	;store wr3
_2:	test	$4,r3
	je	_1
	sub	$1,r2
	mov	wr2,warm(r2)	;store wr2
_1:	test	$2,r3
	je	_0
	sub	$1,r2
	mov	wr1,warm(r2)	;store wr1
_0:	test	$1,r3
	je	donem
	sub	$1,r2
	mov	wr0,warm(r2)	;store wr0

donem:	mov	r2,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

;;; -----EXECUTE INSTRUCTION AND SET CONDITION CODES-----
	
wadds:	add	r3,r2		;--adds--
	mov	ccr,wccr
	mov	r2,wregs(r1)	;--dests--
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip
	
wsubs:	sub	r3,r2		;--subs--
	mov	ccr,wccr
	mov	r2,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wands:	and	r3,r2		;--ands--
	mov	ccr,wccr
	mov	r2,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wmovs:	mov	r3,wregs(r1)	;--movs--
	cmp	$0,r3
	mov	ccr,wccr
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip

wmvns:	xor	$0xFFFFFFFF,r3	;--mvn--
	mov	ccr,wccr
	mov	r3,wregs(r1)
	mov	wpc,r15		; -------------------- BEGIN MAIN LOOP ---------------------
	and	$0xFFFFFF,r15
	mov	warm(r15),r3	; fetch next instruction -> instruction pointer
	mov 	r3, r0		; isolate opcode and "s" with PLA
	trap	$SysPLA
	test	$0xE0000000,r3	; test for always
	cmove	typejmp(r0),rip	; ALWAYS
	mov	r3,r9		; isolate condition
	shr	$29,r9
	mov	conjmp(r9),rip
	
opjmp:
	.data	wadd,wadc,wsub,wcmp,weor,worr,wand,wtst
	.data	wmul,halt,wdiv,wmov,wmvn,halt,wldm,wstm
	.data	wldr,wstr,wldu,wstu,wadr,halt,halt,halt
	.data	halt,halt,halt,halt,halt,halt,halt,halt
	.data	wadds,wadc,wsubs,wcmp,weor,worr,wands,wtst
	.data	wmul,halt,wdiv,wmovs,wmvns,halt,wldm,wstm
	.data	wldr,wstr,wldu,wstu,wadr

wregs:
wr0:	.data	0
wr1:	.data	0
wr2:	.data	0
wr3:	.data	0
wr4:	.data	0
wr5:	.data	0
wr6:	.data	0
wr7:	.data	0
wr8:	.data	0
wr9:	.data	0
wr10:	.data	0
wr11:	.data	0
wr12:	.data	0
wsp:	
wr13:	.data	0x00ffffff
wlr:	
wr14:	.data	0
wpc:	
wr15:	.data	0
wccr:	.data	0
;;; --------------------- write no code below this line -----------------------
warm:				; Warm overlay is loaded here
	
