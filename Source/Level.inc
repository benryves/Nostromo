NodeType.Leaf = 0
NodeType.VerticalPartition = 1
NodeType.HorizontalPartition = 2

Sector0: .dw -64, 128
Sector1: .dw 32, 96
Sector2: .dw -50, 64
Sector3: .dw -96, 192

Vertices:
	.dw 0, 0
	.dw 1536, 0
	.dw 2048, 512
	.dw 3840, 512
	.dw 4608, 1280
	.dw 4608, 2816
	.dw 5120, 3328
	.dw 5120, 5376
	.dw 4608, 5888
	.dw 3840, 5888
	.dw 3328, 5376
	.dw 3328, 3328
	.dw 3840, 2816
	.dw 3840, 1280
	.dw 2048, 1280
	.dw 1536, 1792
	.dw 0, 1792
	.dw 4224, 4608
	.dw 4608, 5376
	.dw 3840, 5376
	.dw 5120, 4608
	.dw 3328, 4608
	.dw 0, 512
	.dw 0, 1280
	.dw -512, 1280
	.dw -512, 512
	.dw 1024, 1792
	.dw 1024, 2304
	.dw 2048, 2304
	.dw 2048, 3840
	.dw -512, 3840
	.dw -512, 2304
	.dw 512, 2304
	.dw 512, 1792
Vertices.Count = ($ - Vertices) / 4

Wall0: .db 0, 1 \ .dw Sector0, Sector0 \ .db 11
Wall1: .db 1, 2 \ .dw Sector0, Sector0 \ .db 11
Wall2: .db 2, 3 \ .dw Sector0, Sector0 \ .db 11
Wall3: .db 3, 4 \ .dw Sector0, Sector0 \ .db 11
Wall4: .db 4, 5 \ .dw Sector0, Sector0 \ .db 11
Wall5: .db 5, 6 \ .dw Sector0, Sector0 \ .db 11
Wall6: .db 6, 7 \ .dw Sector0, Sector0 \ .db 11
Wall7: .db 7, 8 \ .dw Sector0, Sector0 \ .db 11
Wall8: .db 8, 9 \ .dw Sector0, Sector0 \ .db 11
Wall9: .db 9, 10 \ .dw Sector0, Sector0 \ .db 11
Wall10: .db 10, 11 \ .dw Sector0, Sector0 \ .db 11
Wall11: .db 11, 12 \ .dw Sector0, Sector0 \ .db 11
Wall12: .db 12, 13 \ .dw Sector0, Sector0 \ .db 11
Wall13: .db 13, 14 \ .dw Sector0, Sector0 \ .db 11
Wall14: .db 14, 15 \ .dw Sector0, Sector0 \ .db 11
Wall15: .db 15, 16 \ .dw Sector0, Sector0 \ .db 11
Wall16: .db 16, 0 \ .dw Sector0, Sector0 \ .db 11
Wall17: .db 18, 17 \ .dw Sector0, Sector0 \ .db 11
Wall18: .db 19, 18 \ .dw Sector0, Sector0 \ .db 11
Wall19: .db 17, 19 \ .dw Sector0, Sector0 \ .db 11
Wall20: .db 20, 7 \ .dw Sector0, Sector0 \ .db 10
Wall21: .db 10, 21 \ .dw Sector0, Sector0 \ .db 9
Wall22: .db 6, 20 \ .dw Sector0, Sector0 \ .db 9
Wall23: .db 21, 11 \ .dw Sector0, Sector0 \ .db 10
Wall24: .db 22, 0 \ .dw Sector0, Sector0 \ .db 11
Wall25: .db 23, 22 \ .dw Sector0, Sector1 \ .db 20
Wall26: .db 16, 23 \ .dw Sector0, Sector0 \ .db 11
Wall27: .db 23, 24 \ .dw Sector1, Sector0 \ .db 11
Wall28: .db 24, 25 \ .dw Sector1, Sector0 \ .db 11
Wall29: .db 25, 22 \ .dw Sector1, Sector0 \ .db 11
Wall30: .db 33, 16 \ .dw Sector0, Sector0 \ .db 11
Wall31: .db 26, 33 \ .dw Sector0, Sector2 \ .db 20
Wall32: .db 15, 26 \ .dw Sector0, Sector0 \ .db 11
Wall33: .db 26, 27 \ .dw Sector2, Sector0 \ .db 11
Wall34: .db 27, 28 \ .dw Sector3, Sector0 \ .db 11
Wall35: .db 28, 29 \ .dw Sector3, Sector0 \ .db 11
Wall36: .db 29, 30 \ .dw Sector3, Sector0 \ .db 11
Wall37: .db 30, 31 \ .dw Sector3, Sector0 \ .db 11
Wall38: .db 31, 32 \ .dw Sector3, Sector0 \ .db 11
Wall39: .db 32, 33 \ .dw Sector2, Sector0 \ .db 11
Wall40: .db 33, 26 \ .dw Sector2, Sector2 \ .db 20
Wall41: .db 32, 27 \ .dw Sector3, Sector2 \ .db 20
Wall42: .db 27, 32 \ .dw Sector2, Sector2 \ .db 20

SubSector0: .db 9 \ .dw Wall0, Wall1, Wall14, Wall31, Wall30, Wall32, Wall25, Wall24, Wall26
SubSector1: .db 2 \ .dw Wall2, Wall13
SubSector2: .db 1 \ .dw Wall3
SubSector3: .db 2 \ .dw Wall4, Wall12
SubSector4: .db 4 \ .dw Wall5, Wall22, Wall23, Wall11
SubSector5: .db 4 \ .dw Wall7, Wall8, Wall9, Wall18
SubSector6: .db 2 \ .dw Wall21, Wall19
SubSector7: .db 2 \ .dw Wall17, Wall20
SubSector8: .db 3 \ .dw Wall27, Wall28, Wall29
SubSector9: .db 4 \ .dw Wall42, Wall33, Wall39, Wall40
SubSector10: .db 6 \ .dw Wall41, Wall34, Wall35, Wall36, Wall37, Wall38

Tree:
.db NodeType.VerticalPartition
.dw 2048
.dw Tree.F
.dw Tree.B

Tree.F:
.db NodeType.HorizontalPartition
.dw 2816
.dw Tree.F.F
.dw Tree.F.B

Tree.F.F:
.db NodeType.HorizontalPartition
.dw 4608
.dw Tree.F.F.F
.dw Tree.F.F.B

Tree.F.F.F:
.db NodeType.HorizontalPartition
.dw 5376
.dw Tree.F.F.F.F
.dw Tree.F.F.F.B

Tree.F.F.F.F:
.db NodeType.Leaf
.dw SubSector5

Tree.F.F.F.B:
.db NodeType.VerticalPartition
.dw 4224
.dw Tree.F.F.F.B.F
.dw Tree.F.F.F.B.B

Tree.F.F.F.B.F:
.db NodeType.Leaf
.dw SubSector7

Tree.F.F.F.B.B:
.db NodeType.Leaf
.dw SubSector6

Tree.F.F.B:
.db NodeType.Leaf
.dw SubSector4

Tree.F.B:
.db NodeType.HorizontalPartition
.dw 1280
.dw Tree.F.B.F
.dw Tree.F.B.B

Tree.F.B.F:
.db NodeType.Leaf
.dw SubSector3

Tree.F.B.B:
.db NodeType.VerticalPartition
.dw 1024
.dw Tree.F.B.B.F
.dw Tree.F.B.B.B

Tree.F.B.B.F:
.db NodeType.Leaf
.dw SubSector2

Tree.F.B.B.B:
.db NodeType.Leaf
.dw SubSector1

Tree.B:
.db NodeType.HorizontalPartition
.dw 1792
.dw Tree.B.F
.dw Tree.B.B

Tree.B.F:
.db NodeType.HorizontalPartition
.dw 2304
.dw Tree.B.F.F
.dw Tree.B.F.B

Tree.B.F.F:
.db NodeType.Leaf
.dw SubSector10

Tree.B.F.B:
.db NodeType.Leaf
.dw SubSector9

Tree.B.B:
.db NodeType.VerticalPartition
.dw 0
.dw Tree.B.B.F
.dw Tree.B.B.B

Tree.B.B.F:
.db NodeType.Leaf
.dw SubSector0

Tree.B.B.B:
.db NodeType.Leaf
.dw SubSector8

