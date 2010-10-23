LevelScale = 1

Vertices:
	.dw LevelScale * 0, LevelScale * 0
	.dw LevelScale * 1536, LevelScale * 0
	.dw LevelScale * 2048, LevelScale * 512
	.dw LevelScale * 3840, LevelScale * 512
	.dw LevelScale * 4608, LevelScale * 1280
	.dw LevelScale * 4608, LevelScale * 2816
	.dw LevelScale * 5120, LevelScale * 3328
	.dw LevelScale * 5120, LevelScale * 5376
	.dw LevelScale * 4608, LevelScale * 5888
	.dw LevelScale * 3840, LevelScale * 5888
	.dw LevelScale * 3328, LevelScale * 5376
	.dw LevelScale * 3328, LevelScale * 3328
	.dw LevelScale * 3840, LevelScale * 2816
	.dw LevelScale * 3840, LevelScale * 1280
	.dw LevelScale * 2048, LevelScale * 1280
	.dw LevelScale * 1536, LevelScale * 1792
	.dw LevelScale * 0, LevelScale * 1792
	.dw LevelScale * 3840, LevelScale * 4608
	.dw LevelScale * 4608, LevelScale * 4608
	.dw LevelScale * 4224, LevelScale * 5376
	.dw LevelScale * 5120, LevelScale * 4608
	.dw LevelScale * 3328, LevelScale * 4608
Vertices.Count = ($ - Vertices) / 4

Walls:
	.db 1, 0
	.db 2, 1
	.db 3, 2
	.db 4, 3
	.db 5, 4
	.db 6, 5
	.db 7, 6
	.db 8, 7
	.db 9, 8
	.db 10, 9
	.db 11, 10
	.db 12, 11
	.db 13, 12
	.db 14, 13
	.db 15, 14
	.db 16, 15
	.db 0, 16
	.db 17, 18
	.db 18, 19
	.db 19, 17
	.db 7, 20
	.db 21, 10
	.db 20, 6
	.db 11, 21
Walls.Count = ($ - Walls) / 2

.define Wall(n) (Walls + n * 2)

Sector0: .db 5 \ .dw Wall(0), Wall(1), Wall(14), Wall(15), Wall(16)
Sector1: .db 2 \ .dw Wall(2), Wall(13)
Sector2: .db 1 \ .dw Wall(3)
Sector3: .db 2 \ .dw Wall(4), Wall(12)
Sector4: .db 5 \ .dw Wall(5), Wall(22), Wall(23), Wall(11), Wall(17)
Sector5: .db 3 \ .dw  Wall(7), Wall(8), Wall(9)
Sector6: .db 2 \ .dw  Wall(21), Wall(19)
Sector7: .db 2 \ .dw  Wall(18), Wall(20)


NodeType.Leaf = 0
NodeType.VerticalPartition = 1
NodeType.HorizontalPartition = 2

Tree:
.db NodeType.VerticalPartition
.dw LevelScale * 8*256
.dw Tree.F
.dw Tree.B

Tree.B:
.db NodeType.Leaf
.dw Sector0

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
.dw Sector4

Tree.F.F.F:
.db NodeType.HorizontalPartition
.dw LevelScale * 21*256
.dw Tree.F.F.F.F
.dw Tree.F.F.F.B

Tree.F.F.F.F:
.db NodeType.Leaf
.dw Sector5

Tree.F.F.F.B:
.db NodeType.VerticalPartition
.dw LevelScale * 16.5*256
.dw Tree.F.F.F.B.F
.dw Tree.F.F.F.B.B

Tree.F.F.F.B.F:
.db NodeType.Leaf
.dw Sector7

Tree.F.F.F.B.B:
.db NodeType.Leaf
.dw Sector6

Tree.F.B:
.db NodeType.HorizontalPartition
.dw LevelScale * 5*256
.dw Tree.F.B.F
.dw Tree.F.B.B

Tree.F.B.F:
.db NodeType.Leaf
.dw Sector3

Tree.F.B.B:
.db NodeType.VerticalPartition
.dw LevelScale * 4*256
.dw Tree.F.B.B.F
.dw Tree.F.B.B.B

Tree.F.B.B.F:
.db NodeType.Leaf
.dw Sector2
	
Tree.F.B.B.B:
.db NodeType.Leaf
.dw Sector1