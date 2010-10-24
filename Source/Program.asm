.module Program

	jp Main

Engine:
#include "Nostromo/Nostromo.asm"
.echoln strformat("Engine size: {0} bytes.", $-Engine)

Level:
#include "Level.inc"
.echoln strformat("Level size: {0} bytes.", $-Level)

MovementTicks:
	.dw 0

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

Loop:

; --------------------------------------------------------------------------
; Render the world.
; --------------------------------------------------------------------------

	call Nostromo.Render

; --------------------------------------------------------------------------
; Display the result on the screen.
; --------------------------------------------------------------------------

	call ionFastCopy
	ei
	
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
	
	di
	ld hl,(Nostromo.Interrupt.Ticks)
	ld (MovementTicks),hl
	ld hl,0
	ld (Nostromo.Interrupt.Ticks),hl
	ei
	
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


PlotVertices:
	ld hl,saveSScreen
	ld b,Vertices.Count
	
-:	inc hl
	ld a,(hl)
	inc hl
	
	add a,48
	ld d,a
	
	inc hl
	ld a,(hl)
	inc hl
	
	neg
	add a,32
	ld e,a
	
	push bc
	push hl
	
	cp 64
	jr nc,+
	
	ld a,d
	cp 96
	jr nc,+
	
	call ionGetPixel
	or (hl)
	ld (hl),a
	
+:

	pop hl
	pop bc
	
	djnz -
	ret

Forwards.X: .dw 0
Forwards.Y: .dw 0

.endmodule