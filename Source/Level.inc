VertexScaleFactor = 1
Vertices:
	.dw VertexScaleFactor*0, VertexScaleFactor*0
	.dw VertexScaleFactor*1536, VertexScaleFactor*0
	.dw VertexScaleFactor*2048, VertexScaleFactor*512
	.dw VertexScaleFactor*3840, VertexScaleFactor*512
	.dw VertexScaleFactor*4608, VertexScaleFactor*1280
	.dw VertexScaleFactor*4608, VertexScaleFactor*2816
	.dw VertexScaleFactor*5120, VertexScaleFactor*3328
	.dw VertexScaleFactor*5120, VertexScaleFactor*5376
	.dw VertexScaleFactor*4608, VertexScaleFactor*5888
	.dw VertexScaleFactor*3840, VertexScaleFactor*5888
	.dw VertexScaleFactor*3328, VertexScaleFactor*5376
	.dw VertexScaleFactor*3328, VertexScaleFactor*3328
	.dw VertexScaleFactor*3840, VertexScaleFactor*2816
	.dw VertexScaleFactor*3840, VertexScaleFactor*1280
	.dw VertexScaleFactor*2048, VertexScaleFactor*1280
	.dw VertexScaleFactor*1536, VertexScaleFactor*1792
	.dw VertexScaleFactor*0, VertexScaleFactor*1792
	.dw VertexScaleFactor*3840, VertexScaleFactor*4608
	.dw VertexScaleFactor*4608, VertexScaleFactor*4608
	.dw VertexScaleFactor*4224, VertexScaleFactor*5376
	.dw VertexScaleFactor*5120, VertexScaleFactor*4608
	.dw VertexScaleFactor*3328, VertexScaleFactor*4608
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

Sector0: .db 5, 0, 1, 14, 15, 16
Sector1: .db 2, 2, 13
Sector2: .db 1, 3
Sector3: .db 2, 4, 12
Sector4: .db 5, 5, 22, 23, 11, 17
Sector5: .db 3, 7, 8, 9
Sector6: .db 2, 21, 19
Sector7: .db 2, 18, 20

NodeType.Leaf = 0
NodeType.VerticalPartition = 1
NodeType.HorizontalPartition = 2

Tree:
.db NodeType.VerticalPartition
.dw 8*256
.dw Tree.F
.dw Tree.B

Tree.B:
.db NodeType.Leaf
.dw Sector0

Tree.F:
.db NodeType.HorizontalPartition
.dw 11*256
.dw Tree.F.F
.dw Tree.F.B

Tree.F.F:
.db NodeType.HorizontalPartition
.dw 18*256
.dw Tree.F.F.F
.dw Tree.F.F.B

Tree.F.F.B:
.db NodeType.Leaf
.dw Sector4

Tree.F.F.F:
.db NodeType.HorizontalPartition
.dw 21*256
.dw Tree.F.F.F.F
.dw Tree.F.F.F.B

Tree.F.F.F.F:
.db NodeType.Leaf
.dw Sector5

Tree.F.F.F.B:
.db NodeType.VerticalPartition
.dw 16.5*256
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
.dw 5*256
.dw Tree.F.B.F
.dw Tree.F.B.B

Tree.F.B.F:
.db NodeType.Leaf
.dw Sector3

Tree.F.B.B:
.db NodeType.VerticalPartition
.dw 4*256
.dw Tree.F.B.B.F
.dw Tree.F.B.B.B

Tree.F.B.B.F:
.db NodeType.Leaf
.dw Sector2
	
Tree.F.B.B.B:
.db NodeType.Leaf
.dw Sector1