.module Nostromo

Camera.X: .dw 768
Camera.Y: .dw 896
Camera.Z: .dw 0
Camera.Angle: .db 0

#define neg_hl() ld a,h \ cpl \ ld h,a \ ld a,l \ cpl \ ld l,a \ inc hl
#define neg_de() ld a,d \ cpl \ ld d,a \ ld a,e \ cpl \ ld e,a \ inc de
#define neg_bc() ld a,b \ cpl \ ld b,a \ ld a,c \ cpl \ ld c,a \ inc de

#define cpl_hl() ld a,h \ cpl \ ld h,a \ ld a,l \ cpl \ ld l,a
#define cpl_de() ld a,d \ cpl \ ld d,a \ ld a,e \ cpl \ ld e,a
#define cpl_bc() ld a,b \ cpl \ ld b,a \ ld a,c \ cpl \ ld c,a

#include "Maths.asm"
#include "Vertices.asm"
#include "Wall.asm"
#include "Clip.asm"
#include "Sector.asm"
#include "Trapezium.asm"
#include "Tree.asm"

; ==========================================================================
; Render
; --------------------------------------------------------------------------
; Renders the level.
; --------------------------------------------------------------------------
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Render:

; --------------------------------------------------------------------------
; Clear the screen.
; --------------------------------------------------------------------------

	ld hl,plotSScreen
	ld (hl),0
	ld de,plotSScreen+1
	ld bc,767
	ldir

; --------------------------------------------------------------------------
; Clear the completed columns.
; --------------------------------------------------------------------------

	ld hl,CompletedColumns
	ld (hl),0
	ld de,CompletedColumns+1
	ld bc,95
	ldir

; --------------------------------------------------------------------------
; We have 96 columns to draw.
; --------------------------------------------------------------------------
	
	ld a,96
	ld (ColumnsToDraw),a

; --------------------------------------------------------------------------
; Transform the vertices.
; --------------------------------------------------------------------------
	
	ld hl,Vertices
	ld de,saveSScreen
	ld bc,Vertices.Count
	call Vertices.Transform
	
; --------------------------------------------------------------------------
; Walk the BSP tree to render the level.
; --------------------------------------------------------------------------

	ld (Render.Finish+1),sp

	ld ix,Tree
	call Tree.Walk

Render.Finish:
	ld sp,0
	
	ret

.fill (($+$FF)&$FF00)-$
CompletedColumns:
	.fill 96

ColumnsToDraw:
	.db 0

.endmodule