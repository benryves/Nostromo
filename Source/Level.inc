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
	.dw 3840, 4608
	.dw 4608, 4608
	.dw 4224, 5376
	.dw 5120, 4608
	.dw 3328, 4608
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

Sectors:
	.dw Sector0, Sector1, Sector2, Sector3, Sector4, Sector5, Sector6, Sector7
	Sector0: .db 5, 0, 1, 14, 15, 16
	Sector1: .db 2, 2, 13
	Sector2: .db 1, 3
	Sector3: .db 2, 4, 12
	Sector4: .db 5, 5, 22, 23, 11, 17
	Sector5: .db 3, 7, 8, 9
	Sector6: .db 2, 21, 19
	Sector7: .db 2, 18, 20

