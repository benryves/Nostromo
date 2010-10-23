LevelScale = 1

Vertices:
	.dw LevelScale * 0, LevelScale * 0 ; 0
	.dw LevelScale * 1536, LevelScale * 0 ; 1
	.dw LevelScale * 2048, LevelScale * 512 ; 2
	.dw LevelScale * 3840, LevelScale * 512 ; 3
	.dw LevelScale * 4608, LevelScale * 1280 ; 4
	.dw LevelScale * 4608, LevelScale * 2816 ; 5
	.dw LevelScale * 5120, LevelScale * 3328 ; 6
	.dw LevelScale * 5120, LevelScale * 5376 ; 7
	.dw LevelScale * 4608, LevelScale * 5888 ; 8
	.dw LevelScale * 3840, LevelScale * 5888 ; 9
	.dw LevelScale * 3328, LevelScale * 5376 ; 10
	.dw LevelScale * 3328, LevelScale * 3328 ; 11
	.dw LevelScale * 3840, LevelScale * 2816 ; 12
	.dw LevelScale * 3840, LevelScale * 1280 ; 13
	.dw LevelScale * 2048, LevelScale * 1280 ; 14
	.dw LevelScale * 1536, LevelScale * 1792 ; 15
	.dw LevelScale * 0, LevelScale * 1792 ; 16
	.dw LevelScale * 3840, LevelScale * 4608 ; 17
	.dw LevelScale * 4608, LevelScale * 4608 ; 18
	.dw LevelScale * 4224, LevelScale * 5376 ; 19
	.dw LevelScale * 5120, LevelScale * 4608 ; 20
	.dw LevelScale * 3328, LevelScale * 4608 ; 21
	.dw LevelScale * 0 * 256, LevelScale * 2 * 256 ; 22
	.dw LevelScale * 0 * 256, LevelScale * 5 * 256 ; 23
	.dw LevelScale * -1 * 256, LevelScale * 5 * 256 ; 24
	.dw LevelScale * -1 * 256, LevelScale * 2 * 256 ; 25
Vertices.Count = ($ - Vertices) / 4

Sector0: .dw -64, 128
Sector1: .dw 32, 64

WallFlag.DrawLower = $01
WallFlag.DrawMiddle = $02
WallFlag.DrawUpper = $04

Walls:
	.db 1, 0 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (6 << 3) ; 0
	.db 2, 1 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (7 << 3) ; 1
	.db 3, 2 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (15 << 3) ; 2
	.db 4, 3 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (8 << 3) ; 3
	.db 5, 4 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (12 << 3) ; 4
	.db 6, 5 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (10 << 3) ; 5
	.db 7, 6 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (14 << 3) ; 6
	.db 8, 7 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (12 << 3) ; 7
	.db 9, 8 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (14 << 3) ; 8
	.db 10, 9 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (12 << 3) ; 9
	.db 11, 10 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (14 << 3) ; 10
	.db 12, 11 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (10 << 3) ; 11
	.db 13, 12 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (12 << 3) ; 12
	.db 14, 13 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (15 << 3) ; 13
	.db 15, 14 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (7 << 3) ; 14
	.db 16, 15 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (6 << 3) ; 15
	.db 0, 16 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (8 << 3) ; 16
	.db 17, 18 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (2 << 3) ; 17
	.db 18, 19 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (4 << 3) ; 18
	.db 19, 17 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (6 << 3) ; 19
	.db 7, 20 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (14 << 3) ; 20
	.db 21, 10 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (14 << 3) ; 21
	.db 20, 6 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (14 << 3) ; 22
	.db 11, 21 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (14 << 3) ; 23
	.db 0, 22 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (8 << 3) ; 24
	.db 22, 23 \ .dw Sector0, Sector1 \ .db WallFlag.DrawLower | WallFlag.DrawUpper | (8 << 3) ; 25
	.db 23, 16 \ .dw Sector0, Sector0 \ .db WallFlag.DrawMiddle | (8 << 3) ; 26
	.db 24, 23 \ .dw Sector1, Sector0 \ .db WallFlag.DrawMiddle | (4 << 3) ; 27
	.db 25, 24 \ .dw Sector1, Sector0 \ .db WallFlag.DrawMiddle | (15 << 3) ; 28
	.db 22, 25 \ .dw Sector1, Sector0 \ .db WallFlag.DrawMiddle | (4 << 3) ; 29

.define Wall(n) (Walls + n * 7)

SubSector0: .db 7 \ .dw Wall(0), Wall(1), Wall(14), Wall(15), Wall(24), Wall(25), Wall(26)
SubSector1: .db 2 \ .dw Wall(2), Wall(13)
SubSector2: .db 1 \ .dw Wall(3)
SubSector3: .db 2 \ .dw Wall(4), Wall(12)
SubSector4: .db 5 \ .dw Wall(5), Wall(22), Wall(23), Wall(11), Wall(17)
SubSector5: .db 3 \ .dw  Wall(7), Wall(8), Wall(9)
SubSector6: .db 2 \ .dw  Wall(21), Wall(19)
SubSector7: .db 2 \ .dw  Wall(18), Wall(20)
SubSector8: .db 3 \ .dw  Wall(27), Wall(28), Wall(29)


NodeType.Leaf = 0
NodeType.VerticalPartition = 1
NodeType.HorizontalPartition = 2

Tree:
.db NodeType.VerticalPartition
.dw LevelScale * 8*256
.dw Tree.F
.dw Tree.B

Tree.B:
.db NodeType.VerticalPartition
.dw LevelScale * 0*256
.dw Tree.B.F
.dw Tree.B.B

Tree.B.F:
.db NodeType.Leaf
.dw SubSector0

Tree.B.B:
.db NodeType.Leaf
.dw SubSector8

Tree.F:
.db NodeType.HorizontalPartition
.dw LevelScale * 11*256
.dw Tree.F.F
.dw Tree.F.B

Tree.F.F:
.db NodeType.HorizontalPartition
.dw LevelScale * 18*256
.dw Tree.F.F.F
.dw Tree.F.F.B

Tree.F.F.B:
.db NodeType.Leaf
.dw SubSector4

Tree.F.F.F:
.db NodeType.HorizontalPartition
.dw LevelScale * 21*256
.dw Tree.F.F.F.F
.dw Tree.F.F.F.B

Tree.F.F.F.F:
.db NodeType.Leaf
.dw SubSector5

Tree.F.F.F.B:
.db NodeType.VerticalPartition
.dw LevelScale * 16.5*256
.dw Tree.F.F.F.B.F
.dw Tree.F.F.F.B.B

Tree.F.F.F.B.F:
.db NodeType.Leaf
.dw SubSector7

Tree.F.F.F.B.B:
.db NodeType.Leaf
.dw SubSector6

Tree.F.B:
.db NodeType.HorizontalPartition
.dw LevelScale * 5*256
.dw Tree.F.B.F
.dw Tree.F.B.B

Tree.F.B.F:
.db NodeType.Leaf
.dw SubSector3

Tree.F.B.B:
.db NodeType.VerticalPartition
.dw LevelScale * 4*256
.dw Tree.F.B.B.F
.dw Tree.F.B.B.B

Tree.F.B.B.F:
.db NodeType.Leaf
.dw SubSector2
	
Tree.F.B.B.B:
.db NodeType.Leaf
.dw SubSector1