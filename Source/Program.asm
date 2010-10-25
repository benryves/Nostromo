.module Program

	jp Main

Engine:
#include "Nostromo/Nostromo.asm"
.echoln strformat("Engine size: {0} bytes.", $-Engine)

MovementTicks:
	.dw 0

FPSCounter:
	.db 0

Main:

	call Nostromo.Interrupt.Load
	
	ld hl,768
	ld (Nostromo.Camera.X),hl
	ld hl,896
	ld (Nostromo.Camera.Y),hl
	ld hl,0
	ld (Nostromo.Camera.Z),hl
	
	ld a,$00
	ld (Nostromo.Camera.Angle),a
	
	xor a
	ld (FPSCounter),a

Loop:

; --------------------------------------------------------------------------
; Render the world.
; --------------------------------------------------------------------------

	call Nostromo.Render

; --------------------------------------------------------------------------
; Are we displaying the FPSCounter counter?
; --------------------------------------------------------------------------

	ld hl,57*256
	ld (penCol),hl

	set textWrite,(iy+sGrFlags)
	set textEraseBelow,(iy+textFlags)
	set textInverse,(iy+textFlags)
	
	ld a,' '
	.bcall _VPutMap

	ld a,(FPSCounter)
	.bcall _SetXXOP1
	ld a,2
	.bcall _DispOP1A

	ld hl,FPSCounterString
	.bcall _VPutS

	
	res textWrite,(iy+sGrFlags)
	res textEraseBelow,(iy+textFlags)
	res textInverse,(iy+textFlags)

	jr SkipFPSCounter

FPSCounterString:
	.db "FPS",0

SkipFPSCounter:

; --------------------------------------------------------------------------
; Display the result on the screen.
; --------------------------------------------------------------------------

	call ionFastCopy
	
; --------------------------------------------------------------------------
; Fetch the number of ticks.
; --------------------------------------------------------------------------

	di
	ld hl,(Nostromo.Interrupt.Ticks)
	ld (MovementTicks),hl
	ld bc,0
	ld (Nostromo.Interrupt.Ticks),bc
	ei
	
; --------------------------------------------------------------------------
; Calculate the FPSCounter.
; --------------------------------------------------------------------------

	ld c,l
	ld hl,320
	call Nostromo.Maths.Div.U16U8	
	ld a,l
	ld (FPSCounter),a

; --------------------------------------------------------------------------
; Handle input.
; --------------------------------------------------------------------------
	
	ld a,$FF
	out (1),a
	nop
	ld a,$FD
	out (1),a
	nop
	nop
	in a,(1)
	bit 6,a
	jr nz,+	
	im 1
	ret
+:

	ld a,$FF
	out (1),a
	nop
	ld a,$FE
	out (1),a
	nop
	nop
	in a,(1)
	ld c,a
	
	; Check for Left/Right.
	
	ld hl,Nostromo.Camera.Angle	
	
	bit 1,c
	jr nz,+
	ld a,(MovementTicks)
	sra a
	sra a
	sra a
	jr z,+
	add a,(hl)
	ld (hl),a
+:

	bit 2,c
	jr nz,+
	ld a,(MovementTicks)
	neg
	sra a
	sra a
	sra a
	jr z,+
	add a,(hl)
	ld (hl),a
+:

	; Check for Up/Down.
	
	push bc
	
	ld a,(Nostromo.Camera.Angle)
	call Nostromo.Maths.Trig.Sin
	ld de,(MovementTicks)
	call Nostromo.Maths.Mul.S16S16
	sla l \ rl h \ rl e
	ld l,h
	ld h,e	
	ld (Forwards.X),hl
	
	ld a,(Nostromo.Camera.Angle)
	call Nostromo.Maths.Trig.Cos
	ld de,(MovementTicks)
	call Nostromo.Maths.Mul.S16S16
	sla l \ rl h \ rl e
	ld l,h
	ld h,e
	ld (Forwards.Y),hl
	
	pop bc
	
	bit 3,c
	jr nz,+
	
	ld hl,(Nostromo.Camera.X)
	ld de,(Forwards.X)
	add hl,de
	ld (Nostromo.Camera.X),hl
	
	ld hl,(Nostromo.Camera.Y)
	ld de,(Forwards.Y)
	add hl,de
	ld (Nostromo.Camera.Y),hl
	
+:

	bit 0,c
	jr nz,+
	
	ld hl,(Nostromo.Camera.X)
	ld de,(Forwards.X)
	or a
	sbc hl,de
	ld (Nostromo.Camera.X),hl
	
	ld hl,(Nostromo.Camera.Y)
	ld de,(Forwards.Y)
	or a
	sbc hl,de
	ld (Nostromo.Camera.Y),hl
	
+:

	ld a,$FF
	out (1),a
	nop
	ld a,$BF
	out (1),a
	nop
	nop
	in a,(1)
	ld c,a

	; Check for Trace/Graph.

	push bc
	
	ld a,(Nostromo.Camera.Angle)
	call Nostromo.Maths.Trig.Cos
	ld de,(MovementTicks)
	call Nostromo.Maths.Mul.S16S16
	sla l \ rl h \ rl e
	ld l,h
	ld h,e
	ld (Forwards.X),hl
	
	ld a,(Nostromo.Camera.Angle)
	call Nostromo.Maths.Trig.Sin
	ld de,(MovementTicks)
	call Nostromo.Maths.Mul.S16S16
	sla l \ rl h \ rl e
	ld l,h
	ld h,e
	neg_hl()
	ld (Forwards.Y),hl
	
	pop bc

	bit 1,c
	jr nz,+
	
	ld hl,(Nostromo.Camera.X)
	ld de,(Forwards.X)
	add hl,de
	ld (Nostromo.Camera.X),hl
	
	ld hl,(Nostromo.Camera.Y)
	ld de,(Forwards.Y)
	add hl,de
	ld (Nostromo.Camera.Y),hl
	
+:

	bit 0,c
	jr nz,+
	
	ld hl,(Nostromo.Camera.X)
	ld de,(Forwards.X)
	or a
	sbc hl,de
	ld (Nostromo.Camera.X),hl
	
	ld hl,(Nostromo.Camera.Y)
	ld de,(Forwards.Y)
	or a
	sbc hl,de
	ld (Nostromo.Camera.Y),hl
	
+:

	ld hl,(Nostromo.Camera.Z)
	ld de,(MovementTicks)
	sra d \ rr e
	sra d \ rr e

	; Check for Del
	ld a,$FF
	out (1),a
	nop
	ld a,$BF
	out (1),a
	nop
	nop
	in a,(1)
	bit 7,a
	jr nz,+
	add hl,de
+:

	; Check for Stat
	ld a,$FF
	out (1),a
	nop
	ld a,$F7
	out (1),a
	nop
	nop
	in a,(1)
	bit 7,a
	jr nz,+
	or a
	sbc hl,de
+:
	
	ld (Nostromo.Camera.Z),hl

	jp Loop

Forwards.X: .dw 0
Forwards.Y: .dw 0

Level:
#include "Level.inc"
.echoln strformat("Level size: {0} bytes.", $-Level)

.endmodule