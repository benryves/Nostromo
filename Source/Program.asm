.module Program
	jp Main

Engine:
#include "Nostromo/Nostromo.asm"

Main:

	call Nostromo.Initialise
	ret c

	ld hl,Level
	call Nostromo.Level.Load
	jp c,Exit.ShutDownNostromo
	
	xor a
	ld (FPSCounter),a
	ld (CameraAngleTicksRemainder),a
	ld (Sector.Current),a
	ld (Sector.Previous),a
	
	set DemoFlag.FPSCounter,(iy+DemoFlags)
	res DemoFlag.FlyMode,(iy+DemoFlags)
	set DemoFlag.CollisionDetection,(iy+DemoFlags)
	
	xor a
	ld (Menu.SelectedItem.Index),a

	ld hl,0
	ld (Player.Z.Delta),hl

Loop:

; --------------------------------------------------------------------------
; Remember which sector we used to be in.
; --------------------------------------------------------------------------

	ld a,(Sector.Current)
	ld (Sector.Previous),a

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

	ld a,e
	ld hl,335
	.bcall _DivHLByA
	ld a,l
	ld (FPSCounter),a

; --------------------------------------------------------------------------
; Handle input.
; --------------------------------------------------------------------------
	
	; Check for Clear.
	
	ld a,skClear
	call Nostromo.Key.GetState
	jp nc,Exit.UnloadLevel
	
	; Check for Y=.
	
	ld a,skYEqu
	call Nostromo.Key.GetState
	jr c,+
	call Menu
	di
	ld hl,0
	ld (MovementTicks),hl
	ld (Nostromo.Interrupt.Ticks),hl
	ei
+:

	ld hl,(MovementTicks)
	ld de,8
	or a
	sbc hl,de
	jp c,SkipMovementInput

	; Load the current position.
	ld hl,Nostromo.Camera.X
	ld de,Nostromo.Physics.Actor.StartPosition.X
	ldi \ ldi
	ldi \ ldi
	ld hl,Nostromo.Camera.X
	ld de,Nostromo.Physics.Actor.EndPosition.X
	ldi \ ldi
	ldi \ ldi
	
	; Set the actor height.
	ld hl,(Nostromo.Camera.Z)
	ld de,-96
	add hl,de
	ld (Nostromo.Physics.Actor.Z.Feet),hl
	ld de,48
	add hl,de
	ld (Nostromo.Physics.Actor.Z.Knees),hl
	ld de,80
	add hl,de
	ld (Nostromo.Physics.Actor.Z.Head),hl

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
	sla l \ rl h \ rl e
	ld l,h
	ld h,e	
	ld (Forwards.X),hl
	
	ld a,(Nostromo.Camera.Angle)
	call Nostromo.Maths.Trig.Cos
	ld de,(MovementTicks)
	call Nostromo.Maths.Mul.S16S16
	sla l \ rl h \ rl e
	sla l \ rl h \ rl e
	ld l,h
	ld h,e
	ld (Forwards.Y),hl
	
	pop bc
	
	bit 3,c
	jr nz,+
	
	ld hl,(Nostromo.Physics.Actor.EndPosition.X)
	ld de,(Forwards.X)
	add hl,de
	ld (Nostromo.Physics.Actor.EndPosition.X),hl
	
	ld hl,(Nostromo.Physics.Actor.EndPosition.Y)
	ld de,(Forwards.Y)
	add hl,de
	ld (Nostromo.Physics.Actor.EndPosition.Y),hl
	
+:

	bit 0,c
	jr nz,+
	
	ld hl,(Nostromo.Physics.Actor.EndPosition.X)
	ld de,(Forwards.X)
	or a
	sbc hl,de
	ld (Nostromo.Physics.Actor.EndPosition.X),hl
	
	ld hl,(Nostromo.Physics.Actor.EndPosition.Y)
	ld de,(Forwards.Y)
	or a
	sbc hl,de
	ld (Nostromo.Physics.Actor.EndPosition.Y),hl
	
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
	sla l \ rl h \ rl e
	ld l,h
	ld h,e
	ld (Forwards.X),hl
	
	ld a,(Nostromo.Camera.Angle)
	call Nostromo.Maths.Trig.Sin
	ld de,(MovementTicks)
	call Nostromo.Maths.Mul.S16S16
	sla l \ rl h \ rl e
	sla l \ rl h \ rl e
	ld l,h
	ld h,e
	neg_hl()
	ld (Forwards.Y),hl
	
	pop bc

	bit 1,c
	jr nz,+
	
	ld hl,(Nostromo.Physics.Actor.EndPosition.X)
	ld de,(Forwards.X)
	add hl,de
	ld (Nostromo.Physics.Actor.EndPosition.X),hl
	
	ld hl,(Nostromo.Physics.Actor.EndPosition.Y)
	ld de,(Forwards.Y)
	add hl,de
	ld (Nostromo.Physics.Actor.EndPosition.Y),hl
	
+:

	bit 0,c
	jr nz,+
	
	ld hl,(Nostromo.Physics.Actor.EndPosition.X)
	ld de,(Forwards.X)
	or a
	sbc hl,de
	ld (Nostromo.Physics.Actor.EndPosition.X),hl
	
	ld hl,(Nostromo.Physics.Actor.EndPosition.Y)
	ld de,(Forwards.Y)
	or a
	sbc hl,de
	ld (Nostromo.Physics.Actor.EndPosition.Y),hl
	
+:

	ld hl,(Nostromo.Camera.Z)
	ld de,(MovementTicks)
	sra d \ rr e

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
	add hl,de
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
	ld a,d
	add a,e
	cp 64
	jr c,++
	ld a,64
++:
	ld d,a
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

	bit DemoFlag.CollisionDetection,(iy+DemoFlags)
	jr z,CollisionDetection.Skip

	; Attempt to move the camera.
	call Nostromo.Physics.MoveActor

CollisionDetection.Skip:

	; Copy the actor position back to the camera.
	ld hl,Nostromo.Physics.Actor.EndPosition.X
	ld de,Nostromo.Camera.X
	ldi \ ldi
	ldi \ ldi

; --------------------------------------------------------------------------
; Are any sectors moving?
; --------------------------------------------------------------------------

	ld ix,MovingSectors
	ld b,3
MoveSector:
	push bc

	ld e,(ix+4)
	ld d,(ix+5)
	ld a,e
	or d
	jr z,Sector.NotMoving
	
	ld bc,(MovementTicks)
	call Nostromo.Maths.Mul.U16U16

	ld e,(ix+2)
	ld d,(ix+3)
	
	add hl,de
	
	ex de,hl
	
	ld a,d
	xor $80
	ld d,a
	
	ld l,(ix+6)
	ld a,(ix+7)
	xor $80
	ld h,a
	
	or a
	sbc hl,de
	
	jr c,+
	
	add hl,de
	ex de,hl
	
	xor a
	ld (ix+4),a
	ld (ix+5),a
	
+:
	ld l,(ix+8)
	ld a,(ix+9)
	xor $80
	ld h,a
	or a
	sbc hl,de

	jr nc,+
	
	add hl,de
	ex de,hl
	
	xor a
	ld (ix+4),a
	ld (ix+5),a

+:
	ld a,d
	xor $80
	ld d,a
	
	ld (ix+2),e
	ld (ix+3),d

	ld b,4
-:	sra d \ rr e
	djnz -
	
	ld l,(ix+0)
	ld h,(ix+1)
	
	ld (hl),e
	inc hl
	ld (hl),d

Sector.NotMoving:

	ld bc,10
	add ix,bc
	pop bc
	djnz MoveSector

; --------------------------------------------------------------------------
; Are we snapped to the floor?
; --------------------------------------------------------------------------

	bit DemoFlag.FlyMode,(iy+DemoFlags)
	jp nz,NotSnappedToFloor

	.if Nostromo.Options.KeepStatistics
	ld hl,(Nostromo.Statistics.TreeNodesVisited)
	ld (plotSScreen),hl
	.endif

	ld (FindFloorHeightFunction.SP),sp
	ld ix,(Nostromo.Level.Tree)
	ld hl,(Nostromo.Camera.X)
	ld de,(Nostromo.Camera.Y)
	ld bc,FindFloorHeightFunction
	
	call Nostromo.Tree.Walk

FindFloorHeightFunction:
FindFloorHeightFunction.SP = $+1
	ld sp,0

	.if Nostromo.Options.KeepStatistics
	ld hl,(plotSScreen)
	ld (Nostromo.Statistics.TreeNodesVisited),hl
	.endif
	
	; Leaf offset.
	inc ix
	
	; Sector address.
	ld a,(ix)
	
	ld (Sector.Current),a
	
	ld l,a
	ld h,0
	
	add hl,hl
	add hl,hl
	
	ld de,(Nostromo.Level.Sectors)
	
	add hl,de
	
	; Floor height.
	ld e,(hl)
	inc hl
	ld d,(hl)
	
	; Give the player some height.
	ld hl,96
	add hl,de
	
	ld (Player.TargetZ),hl
	
	ld hl,(Nostromo.Camera.Z)
	ld de,(Player.Z.Delta)
	add hl,de
	ld (Nostromo.Camera.Z),hl
	
	ld a,(MovementTicks)
	srl a
	srl a
	jr z,NotSnappedToFloor
	ld b,a
	
HeightPhysicsLoop:
	push bc

	ld hl,(Nostromo.Camera.Z)
	ld a,h \ xor $80 \ ld h,a
	
	ld de,(Player.TargetZ)
	ld a,d \ xor $80 \ ld d,a

	or a
	sbc hl,de
	jr z,NoRiseOrFall

	jr nc,Fall

Rise:

	ld hl,0
	ld (Player.Z.Delta),hl
	
	ld hl,(Player.TargetZ)
	ld de,(Nostromo.Camera.Z)
	sra h \ rr l
	sra d \ rr e
	add hl,de
	ld (Nostromo.Camera.Z),hl
	jr NoRiseOrFall

Fall:
	
	ld hl,(Player.Z.Delta)
	dec hl
	ld (Player.Z.Delta),hl

NoRiseOrFall:
	pop bc
	djnz HeightPhysicsLoop

NotSnappedToFloor:

; --------------------------------------------------------------------------
; Have we moved from one sector to another?
; --------------------------------------------------------------------------

	ld a,(Sector.Previous)
	ld b,a
	ld a,(Sector.Current)
	cp b
	jr z,Sector.NotChanged
	ld c,a

; --------------------------------------------------------------------------
; We've moved from sector B to sector C.
; --------------------------------------------------------------------------
	
	ld a,c
	cp 35
	jr nz,Sector.NotLowerAltar
	ld a,b
	cp 36
	jr nz,Sector.NotLowerAltar

	ld hl,-4
	ld (MovingSectors+4),hl	

Sector.NotLowerAltar:

	ld a,c
	cp 34
	jr nz,Sector.NotRaiseAltar
	ld a,b
	cp 35
	jr nz,Sector.NotRaiseAltar

	ld hl,+4
	ld (MovingSectors+4),hl	

Sector.NotRaiseAltar:

	ld a,c
	cp 40
	jr z,Sector.OpenLeftDoor
	cp 2
	jr nz,Sector.NotOpenLeftDoor
Sector.OpenLeftDoor:
	ld hl,+8
	ld (LeftDoor+4),hl	
Sector.NotOpenLeftDoor:

	ld a,c
	cp 39
	jr z,Sector.OpenRightDoor
	cp 38
	jr nz,Sector.NotOpenRightDoor
Sector.OpenRightDoor:
	ld hl,+8
	ld (RightDoor+4),hl	
Sector.NotOpenRightDoor:

	ld a,c
	cp 5
	jr z,Sector.CloseDoors
	cp 6
	jr z,Sector.CloseDoors
	cp 0
	jr nz,Sector.NotCloseDoors
Sector.CloseDoors:
	ld hl,-4
	ld (LeftDoor+4),hl	
	ld (RightDoor+4),hl	
Sector.NotCloseDoors:

Sector.NotChanged:

; --------------------------------------------------------------------------
; Clear MovementTicks.
; --------------------------------------------------------------------------

	ld hl,0
	ld (MovementTicks),hl
	
SkipMovementInput:

	jp Loop

Forwards.X: .dw 0
Forwards.Y: .dw 0

Exit.UnloadLevel:
	call Nostromo.Level.Unload

Exit.ShutDownNostromo:
	call Nostromo.ShutDown

.if outputwriteris('ti8x')
	; Reset speed.
	in a,($02)
	bit 7,a
	jr z,++
	xor a
	out ($20),a
++:
.endif
	
	ret

Menu:

; --------------------------------------------------------------------------
; Wait for the menu key to be released.
; --------------------------------------------------------------------------

	call Nostromo.Key.GetCurrent
	jr nz,Menu

Menu.InputLoop:

	call Menu.RenderFrame

; --------------------------------------------------------------------------
; Render the menu items.
; --------------------------------------------------------------------------
	
	ld ix,Menu.Items
	
	ld a,7
	ld (penRow),a

	ld a,(Menu.SelectedItem.Index)
	inc a
	ld b,a

Menu.RenderItem:

	set textWrite,(iy+sGrFlags)
	
	ld a,(ix)
	or a
	jr z,Menu.DrawnAllItems

	ld a,(penRow)
	add a,7
	ld (penRow),a
	
	push bc

	push ix
	pop hl
	ld a,2
	ld (penCol),a
	.bcall _VPutS
	push hl
	pop ix
	
	ld l,(ix+0)
	ld h,(ix+1)
	call Menu.CallHL
	inc ix
	inc ix
	
	pop bc	
	
	dec b
	jr nz,+
	ld l,(ix+0)
	ld h,(ix+1)
	ld (Menu.SelectedItem.Action),hl
+:
	inc ix
	inc ix
	
	res textWrite,(iy+sGrFlags)
	jr Menu.RenderItem

Menu.DrawnAllItems:

; --------------------------------------------------------------------------
; Draw the selection rectangle.
; --------------------------------------------------------------------------
	
	ld a,(Menu.SelectedItem.Index)
	ld l,a
	ld h,84
	.bcall _HTimesL
	ld de,plotSScreen+12*14
	add hl,de
	
	ld c,7

--:	ld a,(hl)
	xor %01111111
	ld (hl),a
	inc hl
	
	ld b,10
-:	ld a,(hl)
	cpl
	ld (hl),a
	inc hl
	djnz -

	ld a,(hl)
	xor %11100000
	ld (hl),a
	inc hl
	
	dec c
	jr nz,--
	
; --------------------------------------------------------------------------
; Copy the buffer to the display.
; --------------------------------------------------------------------------
	
	call Nostromo.Screen.Copy
	ei

; --------------------------------------------------------------------------
; Get an input key.
; --------------------------------------------------------------------------

-:	halt
	call Nostromo.Key.GetOneShot
	jr z,-

; --------------------------------------------------------------------------
; Is it Clear/YEqu?
; --------------------------------------------------------------------------

	cp skClear
	jr z,Menu.Exit
	cp skYEqu
	jr nz,Menu.NoExit
Menu.Exit:
	call Nostromo.Key.GetCurrent
	jr nz,Menu.Exit
	ret
Menu.NoExit:

; --------------------------------------------------------------------------
; Is it 2nd/Enter?
; --------------------------------------------------------------------------

	cp sk2nd
	jr z,Menu.Interact
	cp skEnter
	jr nz,Menu.NoInteract
Menu.Interact:
	ld hl,(Menu.SelectedItem.Action)
	call Menu.CallHL
	jp Menu.InputLoop
Menu.NoInteract:

; --------------------------------------------------------------------------
; Is it Up?
; --------------------------------------------------------------------------

	cp skUp
	jr nz,+
	ld a,(Menu.SelectedItem.Index)
	or a
	jp z,Menu.InputLoop
	dec a
	ld (Menu.SelectedItem.Index),a
	jp Menu.InputLoop
+:

; --------------------------------------------------------------------------
; Is it Down?
; --------------------------------------------------------------------------

	cp skDown
	jr nz,+
	ld a,(Menu.SelectedItem.Index)
	inc a
	cp Menu.Items.Count
	jp z,Menu.InputLoop
	ld (Menu.SelectedItem.Index),a
	jp Menu.InputLoop
+:

	jp Menu.InputLoop

Menu.DrawHorizontalLine:
	ld (hl),$FF
	ld d,h
	ld e,l
	inc de
	ld bc,11
	ldir
	ret

Menu.DrawVerticalLine:
	ld de,12
-:	ld a,(hl)
	or c
	ld (hl),a
	add hl,de
	djnz -
	ret

Menu.CallHL:
	jp (hl)

Menu.RenderFrame:
; --------------------------------------------------------------------------
; Clear the graph buffer.
; --------------------------------------------------------------------------

	.bcall _GrBufClr
	
; --------------------------------------------------------------------------
; Draw the horizontal lines.
; --------------------------------------------------------------------------

	ld hl,plotSScreen+12*0
	call Menu.DrawHorizontalLine
	ld hl,plotSScreen+12*13
	call Menu.DrawHorizontalLine
	ld hl,plotSScreen+12*63
	call Menu.DrawHorizontalLine
	
; --------------------------------------------------------------------------
; Draw the vertical lines
; --------------------------------------------------------------------------

	ld hl,plotSScreen+0
	ld bc,64*256+%10000000
	call Menu.DrawVerticalLine
	
	ld hl,plotSScreen+11
	ld bc,64*256+%00000001
	call Menu.DrawVerticalLine

; --------------------------------------------------------------------------
; Draw the caption.
; --------------------------------------------------------------------------

	ld hl,Menu.Header
	ld de,plotSScreen+12*2
	ld bc,Menu.Header.Size
	ldir

; --------------------------------------------------------------------------
; Draw the scroll bar.	
; --------------------------------------------------------------------------
	
	ld hl,plotSScreen+(14*12)+11
	ld de,12
	ld bc,49*256+%00010101
	
-:	ld a,(hl)
	or c
	ld (hl),a
	ld a,c
	xor %00001110
	ld c,a
	add hl,de
	djnz -
	ret

Menu.Items:
.db "Show FPS counter",0 \ .dw Menu.FPSCounter.Render \ .dw Menu.FPSCounter.Interact
.db "CPU speed",0 \ .dw Menu.CPUSpeed.Render \ .dw Menu.CPUSpeed.Interact
.db "Fly mode",0 \ .dw Menu.FlyMode.Render \ .dw Menu.FlyMode.Interact
.db "Collision detection",0 \ .dw Menu.CollisionDetection.Render \ .dw Menu.CollisionDetection.Interact
.db "Render objects",0 \ .dw Menu.RenderThings.Render \ .dw Menu.RenderThings.Interact
.if Nostromo.Options.KeepStatistics
.db "View statistics",0 \ .dw Menu.More.Render \ .dw Menu.ViewStatistics.Interact
.endif
.db 0
Menu.Items.Count = 5 + if(Nostromo.Options.KeepStatistics, 1, 0)

Menu.SelectedItem.Index:
	.db 0

Menu.SelectedItem.Action:
	.dw 0
	
Menu.Nothing.Render:
	ret

Menu.Nothing.Interact:
	ret
	
Menu.More.Render:
	ld hl,Menu.More
	jp Menu.Icon.Draw

Menu.FPSCounter.Render:
	bit DemoFlag.FPSCounter,(iy+DemoFlags)
	jp Menu.CheckBox.Draw

Menu.FPSCounter.Interact:
	ld a,(iy+DemoFlags)
	xor 1 << DemoFlag.FPSCounter
	ld (iy+DemoFlags),a
	ret

Menu.FlyMode.Render:
	bit DemoFlag.FlyMode,(iy+DemoFlags)
	jp Menu.CheckBox.Draw

Menu.FlyMode.Interact:
	ld a,(iy+DemoFlags)
	xor 1 << DemoFlag.FlyMode
	ld (iy+DemoFlags),a
	ret

Menu.CollisionDetection.Render:
	bit DemoFlag.CollisionDetection,(iy+DemoFlags)
	jp Menu.CheckBox.Draw

Menu.CollisionDetection.Interact:
	ld a,(iy+DemoFlags)
	xor 1 << DemoFlag.CollisionDetection
	ld (iy+DemoFlags),a
	ret

Menu.RenderThings.Render:
	bit Nostromo.RenderFlag.DrawThings,(iy+Nostromo.RenderFlags)
	jp Menu.CheckBox.Draw

Menu.RenderThings.Interact:
	ld a,(iy+Nostromo.RenderFlags)
	xor 1 << Nostromo.RenderFlag.DrawThings
	ld (iy+Nostromo.RenderFlags),a
	ret

Menu.CPUSpeed.Render:
	ld hl,CPUSpeed.Slow
.if outputwriteris('ti8x')
	in a,($02)
	bit 7,a
	jr z,+
	
	in a,($20)
	or a
	jr z,+
	ld hl,CPUSpeed.Fast
+:	
.endif
	ld a,70
	ld (penCol),a
	.bcall _VPutS
	ret

CPUSpeed.Slow:
	.db "    6MHz",0
CPUSpeed.Fast:
	.db "15MHz",0

Menu.CPUSpeed.Interact:
.if outputwriteris('ti8x')
	in a,($02)
	bit 7,a
	jr z,+	
	in a,($20)
	xor 1
	out ($20),a
+:	
.endif
	ret

Menu.CheckBox.Draw:
	ld hl,Menu.CheckBox.Unchecked
	jr z,+
	ld hl,Menu.CheckBox.Checked
+:	

Menu.Icon.Draw:
	push hl

	ld a,(penRow)
	inc a
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	ld d,h \ ld e,l
	add hl,hl
	add hl,de
	ld de,plotSScreen+10
	add hl,de
	pop de
	
	ld b,5
--:	push bc
	ld a,(de)
	ld b,5
-:	srl a
	djnz -
	or (hl)
	ld (hl),a
	inc hl
	ld a,(de)
	add a,a
	add a,a
	add a,a
	or (hl)
	ld (hl),a
	ld bc,11
	add hl,bc
	pop bc
	inc de
	djnz --
	
	ret

.if Nostromo.Options.KeepStatistics
Menu.ViewStatistics.Interact:
	call Menu.RenderFrame
	
	set textWrite,(iy+sGrFlags)
	
	ld hl,(0*7+14)*256+2
	ld (penCol),hl
	ld hl,StrPosition
	.bcall _VPutS
	
	ld hl,(Nostromo.Camera.X)
	call DispHLSmall.Signed
	ld a,','
	.bcall _VPutMap
	ld hl,(Nostromo.Camera.Y)
	call DispHLSmall.Signed
	ld a,','
	.bcall _VPutMap
	ld hl,(Nostromo.Camera.Z)
	call DispHLSmall.Signed

	ld hl,(1*7+14)*256+2
	ld (penCol),hl
	ld hl,StrAngle
	.bcall _VPutS
	
	ld a,(Nostromo.Camera.Angle)
	call DispASmall
	ld a,','
	.bcall _VPutMap
	ld a,(Nostromo.Camera.YShear)
	call DispASmall.Signed
	ld a,')'
	.bcall _VPutMap
	
	ld hl,(2*7+14)*256+2
	ld (penCol),hl
	ld hl,StrNodesVisited
	.bcall _VPutS
	ld hl,(Nostromo.Statistics.TreeNodesVisited)
	call DispHLSmall
	
	ld hl,(3*7+14)*256+2
	ld (penCol),hl
	ld hl,StrSubsectorsDrawn
	.bcall _VPutS
	ld hl,(Nostromo.Statistics.SubsectorsDrawn)
	call DispHLSmall
	
	ld hl,(4*7+14)*256+2
	ld (penCol),hl
	ld hl,StrWallsDrawn
	.bcall _VPutS
	ld hl,(Nostromo.Statistics.WallsPotentiallyDrawn)
	call DispHLSmall
	ld a,'/'
	.bcall _VPutMap
	ld hl,(Nostromo.Statistics.WallsActuallyDrawn)
	call DispHLSmall
	
	res textWrite,(iy+sGrFlags)
	
	call Nostromo.Screen.Copy
	ei
-:	halt
	call Nostromo.Key.GetOneShot
	jr z,-
	ret

DispASmall:
	ld l,a
	ld h,0
	jr DispHLSmall

DispASmall.Signed:
	ld l,a
	add a,a
	sbc a,a
	ld h,a

DispHLSmall.Signed:
	bit 7,h
	jr z,DispHLSmall
	push hl
	ld a,'-'
	.bcall _VPutMap
	ld hl,0
	pop de
	or a
	sbc hl,de
	; deliberate run-on
	
DispHLSmall:
	.bcall _SetXXXXOP2
	.bcall _OP2ToOP1
	ld a,5
	.bcall _DispOP1A
	ld hl,penCol
	dec (hl)
	dec (hl)
	ret

StrPosition: .db "P: ", 0
StrAngle: .db "Angle: (", 0
StrNodesVisited: .db "BSP nodes visited: ",0
StrSubsectorsDrawn: .db "Sub-sectors drawn: ",0
StrWallsDrawn: .db "Walls drawn: ",0


.endif

.if $ > $C000
.echoln strformat("PC > $C000 (${0:X4})", $)
.endif

Menu.CheckBox.Unchecked:
	.db %11111000
	.db %10001000
	.db %10001000
	.db %10001000
	.db %11111000

Menu.CheckBox.Checked:
	.db %11111000
	.db %11110000
	.db %10101000
	.db %11011000
	.db %11111000

Menu.More:
	.db %00000000
	.db %00000000
	.db %00000000
	.db %00000000
	.db %10101000

Level:
#include "Level.inc"
.echoln strformat("Level size: {0} bytes.", $-Level)

#include "Things.inc"


MovementTicks:
	.dw 0

CameraAngleTicksRemainder:
	.db 0

FPSCounter:
	.db 0

Player.Z.Delta:
	.dw 0

Player.TargetZ:
	.dw 0

DemoFlags = asm_Flag3
DemoFlag.FPSCounter = 0
DemoFlag.FlyMode = 1
DemoFlag.CollisionDetection = 2

Sector.Previous:
	.db 0

Sector.Current:
	.db 0

MovingSectors:
	.dw Sector34+0, -104 * 16, 0, (-320 + 16) * 16, -104 * 16 ; 0:Ptr to sector variable, 2:current value, 4:delta, 6:minimum value, 8:maximum value.
LeftDoor:
	.dw Sector8+2, -128 * 16, 0, -128 * 16, (64-8) * 16 ; 0:Ptr to sector variable, 2:current value, 4:delta, 6:minimum value, 8:maximum value.
RightDoor:
	.dw Sector9+2, -128 * 16, 0, -128 * 16, (64-8) * 16 ; 0:Ptr to sector variable, 2:current value, 4:delta, 6:minimum value, 8:maximum value.

Menu.Header:
.db $BF,$7E,$FD,$FB,$F7,$EF,$EF,$C0,$00,$00,$00,$FD,$B3,$66,$CC,$63
.db $36,$6D,$6C,$C0,$00,$00,$00,$CD,$B3,$66,$CC,$63,$36,$6D,$6C,$C0
.db $00,$00,$00,$CD,$B3,$66,$C0,$63,$36,$6D,$6C,$C0,$00,$00,$00,$0D
.db $B3,$66,$FC,$63,$C6,$6D,$6C,$C0,$00,$00,$00,$3D,$B3,$66,$0C,$63
.db $36,$6C,$6C,$C0,$DF,$BF,$7E,$31,$B3,$66,$CC,$63,$36,$6C,$6C,$C0
.db $D8,$2B,$46,$31,$B3,$66,$CC,$63,$36,$6C,$6C,$CF,$DF,$AB,$46,$31
.db $B3,$66,$CC,$63,$36,$6C,$6C,$C8,$D8,$23,$46,$31,$B3,$7E,$FC,$63
.db $37,$EC,$6F,$CF,$DF,$A3,$7E,$31
Menu.Header.Size = $-Menu.Header

.include "Nostromo/End.asm"

.endmodule