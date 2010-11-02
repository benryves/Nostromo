	.dw Vertices, Vertices.Count
	.dw Walls, Walls.Count
	.dw SubSectors
	.dw Sectors
	.dw Tree
	.dw 2560, -2560, -32 \ .db 0, 0

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
	.dw -512, -7680
	.dw 1536, -7680
	.dw 1536, -7168
	.dw 3584, -7168
	.dw 3584, -7680
	.dw 5632, -7680
	.dw 5632, -6400
	.dw 5632, -5632
	.dw 5632, -5376
	.dw 4864, -4608
	.dw 4608, -4608
	.dw 4608, -4352
	.dw 3840, -4352
	.dw 3840, -4608
	.dw 3584, -4608
	.dw 3072, -4096
	.dw 2048, -4096
	.dw 1536, -4608
	.dw 1280, -4608
	.dw 1280, -4352
	.dw 512, -4352
	.dw 512, -4608
	.dw 256, -4608
	.dw -512, -5376
	.dw -512, -5632
	.dw -512, -6400
	.dw 0, -6400
	.dw 1536, -6400
	.dw 3584, -6400
	.dw 5120, -6400
	.dw 5120, -5632
	.dw 4608, -5120
	.dw 3584, -5120
	.dw 1536, -5120
	.dw 512, -5120
	.dw 0, -5632
Vertices.Count = ($ - Vertices) / 4

Walls:
	Wall0: .db 7 \ .db 0, 1 \ .dw Sector0, Sector0
	Wall1: .db 7 \ .db 1, 2 \ .dw Sector2, Sector2
	Wall2: .db 3 \ .db 2, 3 \ .dw Sector2, Sector8
	Wall3: .db 7 \ .db 3, 4 \ .dw Sector2, Sector2
	Wall4: .db 7 \ .db 4, 5 \ .dw Sector0, Sector0
	Wall5: .db 5 \ .db 5, 6 \ .dw Sector0, Sector0
	Wall6: .db 4 \ .db 6, 7 \ .dw Sector0, Sector0
	Wall7: .db 6 \ .db 7, 8 \ .dw Sector0, Sector0
	Wall8: .db 7 \ .db 8, 9 \ .dw Sector0, Sector0
	Wall9: .db 7 \ .db 9, 10 \ .dw Sector2, Sector2
	Wall10: .db 3 \ .db 10, 11 \ .dw Sector2, Sector9
	Wall11: .db 7 \ .db 11, 12 \ .dw Sector2, Sector2
	Wall12: .db 7 \ .db 12, 13 \ .dw Sector0, Sector0
	Wall13: .db 7 \ .db 13, 15 \ .dw Sector1, Sector1
	Wall14: .db 6 \ .db 14, 15 \ .dw Sector1, Sector1
	Wall15: .db 4 \ .db 15, 16 \ .dw Sector1, Sector1
	Wall16: .db 5 \ .db 16, 17 \ .dw Sector1, Sector1
	Wall17: .db 4 \ .db 17, 18 \ .dw Sector1, Sector1
	Wall18: .db 6 \ .db 18, 19 \ .dw Sector1, Sector1
	Wall19: .db 4 \ .db 19, 20 \ .dw Sector1, Sector1
	Wall20: .db 7 \ .db 20, 0 \ .dw Sector1, Sector1
	Wall21: .db 6 \ .db 21, 0 \ .dw Sector1, Sector1
	Wall22: .db 3 \ .db 1, 4 \ .dw Sector0, Sector2
	Wall23: .db 3 \ .db 4, 1 \ .dw Sector2, Sector0
	Wall24: .db 3 \ .db 9, 12 \ .dw Sector0, Sector2
	Wall25: .db 3 \ .db 12, 9 \ .dw Sector2, Sector0
	Wall26: .db 3 \ .db 0, 22 \ .dw Sector1, Sector0
	Wall27: .db 3 \ .db 22, 0 \ .dw Sector0, Sector1
	Wall28: .db 3 \ .db 23, 13 \ .dw Sector1, Sector0
	Wall29: .db 3 \ .db 13, 23 \ .dw Sector0, Sector1
	Wall30: .db 3 \ .db 23, 22 \ .dw Sector0, Sector3
	Wall31: .db 3 \ .db 22, 23 \ .dw Sector3, Sector0
	Wall32: .db 3 \ .db 25, 24 \ .dw Sector3, Sector4
	Wall33: .db 0 \ .db 24, 25 \ .dw Sector4, Sector3
	Wall34: .db 3 \ .db 27, 26 \ .dw Sector4, Sector1
	Wall35: .db 0 \ .db 26, 27 \ .dw Sector1, Sector4
	Wall36: .db 0 \ .db 22, 26 \ .dw Sector1, Sector0
	Wall37: .db 1 \ .db 26, 24 \ .dw Sector4, Sector1
	Wall38: .db 2 \ .db 24, 22 \ .dw Sector3, Sector1
	Wall39: .db 0 \ .db 27, 23 \ .dw Sector1, Sector0
	Wall40: .db 1 \ .db 23, 25 \ .dw Sector3, Sector1
	Wall41: .db 2 \ .db 25, 27 \ .dw Sector4, Sector1
	Wall42: .db 7 \ .db 28, 29 \ .dw Sector5, Sector5
	Wall43: .db 7 \ .db 29, 30 \ .dw Sector5, Sector5
	Wall44: .db 7 \ .db 30, 31 \ .dw Sector5, Sector5
	Wall45: .db 7 \ .db 31, 32 \ .dw Sector5, Sector5
	Wall46: .db 7 \ .db 32, 33 \ .dw Sector5, Sector5
	Wall47: .db 5 \ .db 33, 34 \ .dw Sector5, Sector5
	Wall48: .db 4 \ .db 34, 35 \ .dw Sector5, Sector5
	Wall49: .db 6 \ .db 35, 36 \ .dw Sector5, Sector5
	Wall50: .db 7 \ .db 36, 37 \ .dw Sector5, Sector5
	Wall51: .db 7 \ .db 37, 38 \ .dw Sector5, Sector5
	Wall52: .db 7 \ .db 38, 39 \ .dw Sector5, Sector5
	Wall53: .db 7 \ .db 39, 11 \ .dw Sector9, Sector9
	Wall54: .db 7 \ .db 10, 40 \ .dw Sector9, Sector9
	Wall55: .db 7 \ .db 40, 41 \ .dw Sector5, Sector5
	Wall56: .db 7 \ .db 41, 42 \ .dw Sector5, Sector5
	Wall57: .db 7 \ .db 42, 43 \ .dw Sector7, Sector7
	Wall58: .db 7 \ .db 43, 44 \ .dw Sector7, Sector7
	Wall59: .db 7 \ .db 44, 45 \ .dw Sector7, Sector7
	Wall60: .db 3 \ .db 42, 45 \ .dw Sector5, Sector7
	Wall61: .db 7 \ .db 45, 46 \ .dw Sector5, Sector5
	Wall62: .db 7 \ .db 46, 47 \ .dw Sector5, Sector5
	Wall63: .db 7 \ .db 47, 3 \ .dw Sector8, Sector8
	Wall64: .db 7 \ .db 2, 48 \ .dw Sector8, Sector8
	Wall65: .db 7 \ .db 48, 49 \ .dw Sector5, Sector5
	Wall66: .db 7 \ .db 49, 50 \ .dw Sector5, Sector5
	Wall67: .db 7 \ .db 50, 51 \ .dw Sector5, Sector5
	Wall68: .db 5 \ .db 51, 52 \ .dw Sector5, Sector5
	Wall69: .db 4 \ .db 52, 53 \ .dw Sector5, Sector5
	Wall70: .db 6 \ .db 53, 28 \ .dw Sector5, Sector5
	Wall71: .db 1 \ .db 54, 55 \ .dw Sector6, Sector5
	Wall72: .db 0 \ .db 55, 56 \ .dw Sector6, Sector5
	Wall73: .db 2 \ .db 56, 57 \ .dw Sector6, Sector5
	Wall74: .db 3 \ .db 57, 58 \ .dw Sector6, Sector5
	Wall75: .db 3 \ .db 58, 59 \ .dw Sector6, Sector5
	Wall76: .db 1 \ .db 59, 60 \ .dw Sector6, Sector5
	Wall77: .db 0 \ .db 60, 61 \ .dw Sector6, Sector5
	Wall78: .db 2 \ .db 61, 62 \ .dw Sector6, Sector5
	Wall79: .db 3 \ .db 62, 63 \ .dw Sector6, Sector5
	Wall80: .db 3 \ .db 63, 54 \ .dw Sector6, Sector5
	Wall81: .db 3 \ .db 40, 39 \ .dw Sector9, Sector5
	Wall82: .db 3 \ .db 48, 47 \ .dw Sector8, Sector5
Walls.Count = ($ - Walls) / 7

Sectors:
Sector0: .dw -128, 256
Sector1: .dw 0, 256
Sector2: .dw -128, 64
Sector3: .dw -86, 256
Sector4: .dw -42, 256
Sector5: .dw -128, 64
Sector6: .dw -160, 64
Sector7: .dw -96, 64
Sector8: .dw -128, 64
Sector9: .dw -128, 64

SubSectors:
SubSector0: .dw Sector0
	.db 0
	.db 4 \ .dw Wall5, Wall0, Wall4, Wall22

SubSector1: .dw Sector2
	.db 0
	.db 4 \ .dw Wall1, Wall2, Wall3, Wall23

SubSector2: .dw Sector0
	.db 0
	.db 4 \ .dw Wall7, Wall8, Wall12, Wall24

SubSector3: .dw Sector2
	.db 0
	.db 4 \ .dw Wall9, Wall10, Wall11, Wall25

SubSector4: .dw Sector0
	.db 0
	.db 1 \ .dw Wall26

SubSector5: .dw Sector1
	.db 1 \ .dw Thing0
	.db 6 \ .dw Wall37, Wall38, Wall20, Wall18, Wall26, Wall19

SubSector6: .dw Sector0
	.db 0
	.db 1 \ .dw Wall28

SubSector7: .dw Sector1
	.db 0
	.db 6 \ .dw Wall13, Wall40, Wall41, Wall16, Wall28, Wall15

SubSector8: .dw Sector0
	.db 0
	.db 2 \ .dw Wall30, Wall6

SubSector9: .dw Sector3
	.db 0
	.db 4 \ .dw Wall30, Wall32, Wall38, Wall40

SubSector10: .dw Sector4
	.db 1 \ .dw Thing2
	.db 4 \ .dw Wall32, Wall34, Wall37, Wall41

SubSector11: .dw Sector1
	.db 0
	.db 2 \ .dw Wall34, Wall17

SubSector12: .dw Sector5
	.db 0
	.db 2 \ .dw Wall72, Wall44

SubSector13: .dw Sector6
	.db 0
	.db 2 \ .dw Wall72, Wall77

SubSector14: .dw Sector5
	.db 0
	.db 2 \ .dw Wall77, Wall60

SubSector15: .dw Sector7
	.db 1 \ .dw Thing1
	.db 4 \ .dw Wall60, Wall57, Wall58, Wall59

SubSector16: .dw Sector5
	.db 0
	.db 4 \ .dw Wall71, Wall42, Wall43, Wall70

SubSector17: .dw Sector5
	.db 0
	.db 2 \ .dw Wall69, Wall80

SubSector18: .dw Sector6
	.db 0
	.db 2 \ .dw Wall71, Wall80

SubSector19: .dw Sector6
	.db 0
	.db 1 \ .dw Wall79

SubSector20: .dw Sector5
	.db 0
	.db 4 \ .dw Wall66, Wall67, Wall68, Wall79

SubSector21: .dw Sector6
	.db 0
	.db 1 \ .dw Wall78

SubSector22: .dw Sector5
	.db 0
	.db 2 \ .dw Wall78, Wall61

SubSector23: .dw Sector6
	.db 0
	.db 2 \ .dw Wall73, Wall74

SubSector24: .dw Sector5
	.db 0
	.db 2 \ .dw Wall74, Wall48

SubSector25: .dw Sector6
	.db 0
	.db 1 \ .dw Wall76

SubSector26: .dw Sector6
	.db 0
	.db 1 \ .dw Wall75

SubSector27: .dw Sector5
	.db 0
	.db 4 \ .dw Wall75, Wall49, Wall50, Wall51

SubSector28: .dw Sector5
	.db 0
	.db 2 \ .dw Wall76, Wall56

SubSector29: .dw Sector5
	.db 0
	.db 4 \ .dw Wall73, Wall45, Wall46, Wall47

SubSector30: .dw Sector5
	.db 0
	.db 3 \ .dw Wall81, Wall52, Wall55

SubSector31: .dw Sector5
	.db 0
	.db 3 \ .dw Wall82, Wall62, Wall65

SubSector32: .dw Sector8
	.db 0
	.db 4 \ .dw Wall2, Wall82, Wall63, Wall64

SubSector33: .dw Sector9
	.db 0
	.db 4 \ .dw Wall10, Wall53, Wall54, Wall81


Things:
Thing0: .dw 1024, -1280
Thing1: .dw 2560, -4352
Thing2: .dw 2816, -768

Tree:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F
.dw Tree.B
.dw -4096

Tree.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F
.dw Tree.F.B
.dw -3072

Tree.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F
.dw Tree.F.F.B
.dw 2048

Tree.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.F
.dw Tree.F.F.F.B
.dw 3072

Tree.F.F.F.F:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.F.F.F
.dw Tree.F.F.F.F.B
.dw 768
.dw -192

Tree.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector7

Tree.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector6

Tree.F.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.B.F
.dw Tree.F.F.F.B.B
.dw -1024

Tree.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.B.F.F
.dw Tree.F.F.F.B.F.B
.dw -512

Tree.F.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector11

Tree.F.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector10

Tree.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.B.B.F
.dw Tree.F.F.F.B.B.B
.dw -1536

Tree.F.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector9

Tree.F.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector8

Tree.F.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.B.F
.dw Tree.F.F.B.B
.dw -3072
.dw 192

Tree.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector5

Tree.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector4

Tree.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.F
.dw Tree.F.B.B
.dw 2560

Tree.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F
.dw Tree.F.B.F.B
.dw -3584

Tree.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector2

Tree.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector3

Tree.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.B.F
.dw Tree.F.B.B.B
.dw -3584

Tree.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector0

Tree.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector1

Tree.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F
.dw Tree.B.B
.dw 1536

Tree.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.F
.dw Tree.B.F.B
.dw 3584

Tree.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.F
.dw Tree.B.F.F.B
.dw -5632

Tree.B.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.F.F.F
.dw Tree.B.F.F.F.B
.dw 4608

Tree.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.B.F.F.F.F.F
.dw Tree.B.F.F.F.F.B
.dw -512
.dw 256

Tree.B.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector27

Tree.B.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector26

Tree.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.F.B.F
.dw Tree.B.F.F.F.B.B
.dw -4608

Tree.B.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.F.B.F.F
.dw Tree.B.F.F.F.B.F.B
.dw -4352

Tree.B.F.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector33

Tree.B.F.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector30

Tree.B.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.F.B.B.F
.dw Tree.B.F.F.F.B.B.B
.dw -5120

Tree.B.F.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector28

Tree.B.F.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector25

Tree.B.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.B.F
.dw Tree.B.F.F.B.B
.dw -6400

Tree.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.F.B.F.F
.dw Tree.B.F.F.B.F.B
.dw 5120

Tree.B.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector24

Tree.B.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector23

Tree.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector29

Tree.B.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F
.dw Tree.B.F.B.B
.dw -5120

Tree.B.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.F
.dw Tree.B.F.B.F.B
.dw -4608

Tree.B.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector15

Tree.B.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector14

Tree.B.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.B.F
.dw Tree.B.F.B.B.B
.dw -6400

Tree.B.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector13

Tree.B.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector12

Tree.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.F
.dw Tree.B.B.B
.dw -5632

Tree.B.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.F.F
.dw Tree.B.B.F.B
.dw 512

Tree.B.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.F.F.F
.dw Tree.B.B.F.F.B
.dw -4608

Tree.B.B.F.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.F.F.F.F
.dw Tree.B.B.F.F.F.B
.dw -4352

Tree.B.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector32

Tree.B.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector31

Tree.B.B.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.F.F.B.F
.dw Tree.B.B.F.F.B.B
.dw -5120

Tree.B.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector22

Tree.B.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector21

Tree.B.B.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.B.B.F.B.F
.dw Tree.B.B.F.B.B
.dw 0
.dw -256

Tree.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector20

Tree.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector19

Tree.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.F
.dw Tree.B.B.B.B
.dw -6400

Tree.B.B.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.B.F.F
.dw Tree.B.B.B.F.B
.dw 0

Tree.B.B.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector18

Tree.B.B.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector17

Tree.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector16

