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

.endmodule