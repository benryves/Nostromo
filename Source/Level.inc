Sector0: .dw -128, 192
Sector1: .dw 0, 192
Sector2: .dw -128, 64
Sector3: .dw -86, 192
Sector4: .dw -42, 192
Sector5: .dw 0, 0
Sector6: .dw 0, 0

Vertices:
	.dw 0, -3072
	.dw 512, -3584
	.dw 512, -4096
	.dw 1280, -4096
	.dw 1280, -3584
	.dw 1792, -3072
	.dw 2048, -3072
	.dw 3072, -3072
	.dw 3328, -3072
	.dw 3840, -3584
	.dw 3840, -4096
	.dw 4608, -4096
	.dw 4608, -3584
	.dw 5120, -3072
	.dw 5120, -1536
	.dw 5120, -512
	.dw 4608, 0
	.dw 3072, 0
	.dw 2048, 0
	.dw 512, 0
	.dw 0, -512
	.dw 0, -1536
	.dw 2048, -1536
	.dw 3072, -1536
	.dw 2048, -1024
	.dw 3072, -1024
	.dw 2048, -512
	.dw 3072, -512
Vertices.Count = ($ - Vertices) / 4

Wall0: .db 0, 1 \ .dw Sector0, Sector0 \ .db 7
Wall1: .db 1, 2 \ .dw Sector2, Sector2 \ .db 7
Wall2: .db 2, 3 \ .dw Sector2, Sector2 \ .db 7
Wall3: .db 3, 4 \ .dw Sector2, Sector2 \ .db 7
Wall4: .db 4, 5 \ .dw Sector0, Sector0 \ .db 7
Wall5: .db 5, 6 \ .dw Sector0, Sector0 \ .db 5
Wall6: .db 6, 7 \ .dw Sector0, Sector0 \ .db 4
Wall7: .db 7, 8 \ .dw Sector0, Sector0 \ .db 6
Wall8: .db 8, 9 \ .dw Sector0, Sector0 \ .db 7
Wall9: .db 9, 10 \ .dw Sector2, Sector2 \ .db 7
Wall10: .db 10, 11 \ .dw Sector2, Sector2 \ .db 7
Wall11: .db 11, 12 \ .dw Sector2, Sector2 \ .db 7
Wall12: .db 12, 13 \ .dw Sector0, Sector0 \ .db 7
Wall13: .db 13, 14 \ .dw Sector1, Sector1 \ .db 5
Wall14: .db 14, 15 \ .dw Sector1, Sector1 \ .db 6
Wall15: .db 15, 16 \ .dw Sector1, Sector1 \ .db 7
Wall16: .db 16, 17 \ .dw Sector1, Sector1 \ .db 5
Wall17: .db 17, 18 \ .dw Sector1, Sector1 \ .db 4
Wall18: .db 18, 19 \ .dw Sector1, Sector1 \ .db 6
Wall19: .db 19, 20 \ .dw Sector1, Sector1 \ .db 7
Wall20: .db 20, 21 \ .dw Sector1, Sector1 \ .db 5
Wall21: .db 21, 0 \ .dw Sector1, Sector1 \ .db 6
Wall22: .db 1, 4 \ .dw Sector0, Sector2 \ .db 3
Wall23: .db 4, 1 \ .dw Sector2, Sector0 \ .db 3
Wall24: .db 9, 12 \ .dw Sector0, Sector2 \ .db 3
Wall25: .db 12, 9 \ .dw Sector2, Sector0 \ .db 3
Wall26: .db 0, 22 \ .dw Sector1, Sector0 \ .db 3
Wall27: .db 22, 0 \ .dw Sector0, Sector1 \ .db 3
Wall28: .db 23, 13 \ .dw Sector1, Sector0 \ .db 3
Wall29: .db 13, 23 \ .dw Sector0, Sector1 \ .db 3
Wall30: .db 23, 22 \ .dw Sector0, Sector3 \ .db 3
Wall31: .db 22, 23 \ .dw Sector3, Sector0 \ .db 0
Wall32: .db 25, 24 \ .dw Sector3, Sector4 \ .db 3
Wall33: .db 24, 25 \ .dw Sector4, Sector3 \ .db 0
Wall34: .db 27, 26 \ .dw Sector4, Sector1 \ .db 3
Wall35: .db 26, 27 \ .dw Sector1, Sector4 \ .db 0
Wall36: .db 22, 26 \ .dw Sector1, Sector0 \ .db 0
Wall37: .db 26, 24 \ .dw Sector4, Sector1 \ .db 1
Wall38: .db 24, 22 \ .dw Sector3, Sector1 \ .db 2
Wall39: .db 27, 23 \ .dw Sector1, Sector0 \ .db 0
Wall40: .db 23, 25 \ .dw Sector3, Sector1 \ .db 1
Wall41: .db 25, 27 \ .dw Sector4, Sector1 \ .db 2

SubSector0: .db 4 \ .dw Wall5, Wall0, Wall4, Wall22
SubSector1: .db 4 \ .dw Wall1, Wall2, Wall3, Wall23
SubSector2: .db 4 \ .dw Wall7, Wall8, Wall12, Wall24
SubSector3: .db 4 \ .dw Wall9, Wall10, Wall11, Wall25
SubSector4: .db 1 \ .dw Wall27
SubSector5: .db 6 \ .dw Wall21, Wall36, Wall20, Wall19, Wall18, Wall26
SubSector6: .db 1 \ .dw Wall29
SubSector7: .db 6 \ .dw Wall13, Wall39, Wall14, Wall15, Wall16, Wall28
SubSector8: .db 2 \ .dw Wall30, Wall6
SubSector9: .db 4 \ .dw Wall31, Wall32, Wall38, Wall40
SubSector10: .db 4 \ .dw Wall33, Wall34, Wall37, Wall41
SubSector11: .db 2 \ .dw Wall35, Wall17

Tree:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F
.dw Tree.B
.dw -3072

Tree.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F
.dw Tree.F.B
.dw 2048

Tree.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F
.dw Tree.F.F.B
.dw 3072

Tree.F.F.F:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.F.F
.dw Tree.F.F.F.B
.dw 768
.dw -192

Tree.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector7

Tree.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector6

Tree.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.F
.dw Tree.F.F.B.B
.dw -1024

Tree.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.F.F
.dw Tree.F.F.B.F.B
.dw -512

Tree.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector11

Tree.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector10

Tree.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.B.F
.dw Tree.F.F.B.B.B
.dw -1536

Tree.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector9

Tree.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector8

Tree.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.B.F
.dw Tree.F.B.B
.dw -3072
.dw 192

Tree.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector5

Tree.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector4

Tree.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F
.dw Tree.B.B
.dw 2560

Tree.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F
.dw Tree.B.F.B
.dw -3584

Tree.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector2

Tree.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector3

Tree.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.F
.dw Tree.B.B.B
.dw -3584

Tree.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector0

Tree.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector1

Level.Camera.X = 2560
Level.Camera.Y = -2560
Level.Camera.Z = -32
Level.Camera.Angle = 0
