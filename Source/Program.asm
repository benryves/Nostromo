.module Program

	jp Main

Engine:
#include "Nostromo/Nostromo.asm"
.echoln strformat("Engine size: {0} bytes.", $-Engine)

MovementTicks:
	.dw 0

CameraAngleTicksRemainder:
	.db 0

FPSCounter:
	.db 0

DemoFlags = asm_Flag3
DemoFlag.FPSCounter = 0
DemoFlag.YEquHeld = 1
DemoFlag.ZoomHeld = 2
DemoFlag.WindowHeld = 3
DemoFlag.SnapToFloor = 4

Main:
	call Nostromo.Initialise
	ret c
	
	ld hl,Level.Camera.X
	ld (Nostromo.Camera.X),hl
	ld hl,Level.Camera.Y
	ld (Nostromo.Camera.Y),hl
	ld hl,Level.Camera.Z
	ld (Nostromo.Camera.Z),hl
	
	ld a,Level.Camera.Angle
	ld (Nostromo.Camera.Angle),a
	
	xor a
	ld (FPSCounter),a
	ld (Nostromo.Camera.YShear),a
	ld (CameraAngleTicksRemainder),a
	
	set DemoFlag.FPSCounter,(iy+DemoFlags)
	set DemoFlag.YEquHeld,(iy+DemoFlags)
	set DemoFlag.ZoomHeld,(iy+DemoFlags)
	set DemoFlag.WindowHeld,(iy+DemoFlags)
	res DemoFlag.SnapToFloor,(iy+DemoFlags)

Loop:

; --------------------------------------------------------------------------
; Are we snapped to the floor?
; --------------------------------------------------------------------------

	bit DemoFlag.SnapToFloor,(iy+DemoFlags)
	jr z,NotSnappedToFloor

	ld (FindFloorHeightFunction.SP),sp
	ld ix,Tree
	ld hl,(Nostromo.Camera.X)
	ld de,(Nostromo.Camera.Y)
	ld bc,FindFloorHeightFunction
	call Nostromo.Tree.Walk

FindFloorHeightFunction:
FindFloorHeightFunction.SP = $+1
	ld sp,0
	
	; Leaf offset.
	ld l,(ix+Nostromo.Tree.Node.Leaf+0)
	ld h,(ix+Nostromo.Tree.Node.Leaf+1)
	
	; Sector address.
	ld e,(hl)
	inc hl
	ld d,(hl)
	
	ex de,hl
	
	; Floor height.
	ld e,(hl)
	inc hl
	ld d,(hl)
	
	; Give the player some height.
	ld hl,96
	add hl,de
	
	; Set the camera height.
	ld (Nostromo.Camera.Z),hl

NotSnappedToFloor:

; --------------------------------------------------------------------------
; Render the world.
; --------------------------------------------------------------------------

	call Nostromo.Render

; --------------------------------------------------------------------------
; Are we displaying the FPSCounter counter?
; --------------------------------------------------------------------------

	bit DemoFlag.FPSCounter,(iy+DemoFlags)
	jr z,SkipFPSCounter
	
	ld a,(FPSCounter)
	or a
	jr z,SkipFPSCounter

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

	call Nostromo.Screen.Copy
	
; --------------------------------------------------------------------------
; Fetch the number of ticks.
; --------------------------------------------------------------------------


	di
	ld hl,(MovementTicks)
	ld de,(Nostromo.Interrupt.Ticks)
	add hl,de
	ld (MovementTicks),hl
	
	ld bc,0
	ld (Nostromo.Interrupt.Ticks),bc
	ei
	
; --------------------------------------------------------------------------
; Calculate the FPSCounter.
; --------------------------------------------------------------------------

	ld b,0
	ld c,e
	ld hl,335
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
	
.if outputwriteris('ti8x')
	; Reset speed.
	in a,($02)
	bit 7,a
	jr z,++
	xor a
	out ($20),a
++:
.endif

	call Nostromo.ShutDown
	ret
+:

	ld hl,(MovementTicks)
	ld de,8
	or a
	sbc hl,de
	jp c,SkipMovementInput


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
	
	ld hl,(MovementTicks)
	ld de,(CameraAngleTicksRemainder)
	ld d,0
	add hl,de
	ld a,l
	and 7
	ld (CameraAngleTicksRemainder),a
	ld a,l
	sra a
	sra a
	sra a
	ld e,a
	
	ld hl,Nostromo.Camera.Angle
	
	bit 1,c
	jr nz,+
	ld a,e
	add a,(hl)
	ld (hl),a
+:

	bit 2,c
	jr nz,+
	ld a,e
	neg
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

	; Check for Y=
	bit 4,c
	jr nz,+
	bit DemoFlag.YEquHeld,(iy+DemoFlags)
	jr nz,++
	set DemoFlag.YEquHeld,(iy+DemoFlags)
	ld a,(iy+DemoFlags)
	xor 1 << DemoFlag.FPSCounter
	ld (iy+DemoFlags),a
	jr ++
+:	res DemoFlag.YEquHeld,(iy+DemoFlags)
++:

.if outputwriteris('ti8x')
	; Check for Zoom
	bit 2,c
	jr nz,+
	bit DemoFlag.ZoomHeld,(iy+DemoFlags)
	jr nz,++
	set DemoFlag.ZoomHeld,(iy+DemoFlags)
	
	; Toggle speed.
	in a,($02)
	bit 7,a
	jr z,++
	
	in a,($20)
	xor 1
	out ($20),a
	
	jr ++
+:	res DemoFlag.ZoomHeld,(iy+DemoFlags)
++:
.endif

	; Check for Window
	bit 3,c
	jr nz,+
	bit DemoFlag.WindowHeld,(iy+DemoFlags)
	jr nz,++
	set DemoFlag.WindowHeld,(iy+DemoFlags)
	
	ld a,(iy+DemoFlags)
	xor 1 << DemoFlag.SnapToFloor
	ld (iy+DemoFlags),a
	
	jr ++
+:	res DemoFlag.WindowHeld,(iy+DemoFlags)
++:


	ld hl,(Nostromo.Camera.Z)
	ld de,(MovementTicks)
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
	
	
	ld a,(Nostromo.Camera.YShear)
	add a,32
	
	ld de,(MovementTicks)
	sra d \ rr e
	sra d \ rr e
	sra d \ rr e
	ld d,a

	; Check for Mode
	ld a,$FF
	out (1),a
	nop
	ld a,$BF
	out (1),a
	nop
	nop
	in a,(1)
	bit 6,a
	jr nz,+
	ld a,d
	add a,e
	cp 64
	jr c,++
	ld a,64
++:
	ld d,a
+:

	; Check for XT0n
	ld a,$FF
	out (1),a
	nop
	ld a,$EF
	out (1),a
	nop
	nop
	in a,(1)
	bit 7,a
	jr nz,+
	ld a,d
	sub e
	jp p,++
	xor a
++:
	ld d,a
+:
	
	ld a,d
	sub 32
	ld (Nostromo.Camera.YShear),a

	ld hl,0
	ld (MovementTicks),hl

SkipMovementInput:

	jp Loop

Forwards.X: .dw 0
Forwards.Y: .dw 0

Level:
#include "Level.inc"
.echoln strformat("Level size: {0} bytes.", $-Level)

TransformedVertices:
.fill Vertices.Count * 4

.include "Nostromo/End.asm"
.endmodule