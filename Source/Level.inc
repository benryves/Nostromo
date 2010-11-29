	.dw Vertices, Vertices.Count
	.dw Walls, Walls.Count
	.dw Sectors
	.dw Tree
	.dw Things
	.dw BlockMap
	.dw 6912, -8448, -272 \ .db 192, 0

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
	.dw 5120, -512
	.dw 4608, 0
	.dw 3072, 0
	.dw 2048, 0
	.dw 512, 0
	.dw 0, -512
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
	.dw 2304, -7168
	.dw 2304, -7680
	.dw 2304, -8192
	.dw 1792, -8192
	.dw 1280, -8192
	.dw 768, -8192
	.dw 256, -8192
	.dw -768, -8192
	.dw -768, -9216
	.dw -768, -9728
	.dw -768, -10240
	.dw -768, -11264
	.dw 256, -11264
	.dw 256, -10240
	.dw 256, -9728
	.dw 256, -9216
	.dw 768, -9216
	.dw 1280, -9216
	.dw 1792, -9216
	.dw 2304, -9216
	.dw 3328, -9216
	.dw 3328, -8192
	.dw 3328, -7680
	.dw 3328, -7168
	.dw -768, -12288
	.dw 256, -12288
	.dw 768, -12288
	.dw 2304, -10496
	.dw 3584, -10496
	.dw 4352, -10496
	.dw 5120, -11264
	.dw 5888, -11264
	.dw 5888, -10496
	.dw 5888, -8960
	.dw 7168, -8960
	.dw 7168, -7936
	.dw 5888, -7936
	.dw 3584, -7936
	.dw 3584, -9472
	.dw 2304, -9472
	.dw 1280, -9472
	.dw 768, -11264
	.dw 6144, -8960
	.dw 6144, -9472
	.dw 6144, -9984
	.dw 6144, -11264
	.dw 7168, -11264
	.dw 7680, -11264
	.dw 8192, -11264
	.dw 8704, -11264
	.dw 10240, -11264
	.dw 10240, -9984
	.dw 10240, -9472
	.dw 10240, -8960
	.dw 10240, -7936
	.dw 9472, -7936
	.dw 9472, -8448
	.dw 8704, -8448
	.dw 8704, -8960
	.dw 8704, -9472
	.dw 8704, -9984
	.dw 8192, -9984
	.dw 7680, -9984
	.dw 7168, -9984
	.dw 7168, -9472
	.dw 10240, -8448
	.dw 11008, -7936
	.dw 11776, -7936
	.dw 11776, -6400
	.dw 11776, -5632
	.dw 11776, -4864
	.dw 11776, -3072
	.dw 7936, -3072
	.dw 7936, -4864
	.dw 7936, -5632
	.dw 7936, -6400
	.dw 7936, -7936
	.dw 8704, -7936
	.dw 8704, -6400
	.dw 9472, -5632
	.dw 10240, -5632
	.dw 11008, -6400
	.dw 9472, -6656
	.dw 9728, -6400
	.dw 9984, -6400
	.dw 10240, -6656
	.dw 8704, -4864
	.dw 11008, -4864
	.dw 7424, -4864
	.dw 7424, -4352
	.dw 7936, -4352
	.dw 7936, -3584
	.dw 7424, -3584
	.dw 7424, -2560
	.dw 9472, -2560
	.dw 9472, -3072
	.dw 10240, -3072
	.dw 10240, -2560
	.dw 12032, -2560
	.dw 12544, -3072
	.dw 13056, -3072
	.dw 13056, -3328
	.dw 13824, -3328
	.dw 13824, -3072
	.dw 14336, -3072
	.dw 14848, -2560
	.dw 14848, -1280
	.dw 14336, -768
	.dw 13824, -768
	.dw 13056, -768
	.dw 12544, -768
	.dw 12032, -1280
	.dw 7424, -1280
	.dw 6144, -1280
	.dw 6144, -4864
	.dw 13056, -2304
	.dw 13824, -2304
	.dw 13824, -1536
	.dw 13056, -1536
	.dw 9728, -1280
	.dw 13056, -3584
	.dw 13056, -3840
	.dw 12032, -3840
	.dw 12032, -5888
	.dw 12032, -7936
	.dw 13568, -7936
	.dw 14848, -7936
	.dw 15360, -7936
	.dw 16128, -7936
	.dw 16640, -7424
	.dw 16640, -6912
	.dw 16128, -6144
	.dw 16128, -5632
	.dw 16640, -4864
	.dw 16640, -4352
	.dw 16128, -3840
	.dw 15360, -3840
	.dw 14848, -3840
	.dw 13824, -3840
	.dw 13824, -3584
	.dw 13568, -6400
	.dw 14848, -6400
	.dw 15360, -6144
	.dw 15360, -5632
	.dw 14848, -5376
	.dw 13568, -5376
Vertices.Count = ($ - Vertices) / 4

Walls:
	Wall0: .db 7 \ .db 32 \ .db 0, 1 \ .db 0, 0 \ .dw 8
	Wall1: .db 7 \ .db 64 \ .db 1, 2 \ .db 2, 2 \ .dw 4
	Wall2: .db 3 \ .db 0 \ .db 2, 3 \ .db 2, 8 \ .dw 9
	Wall3: .db 7 \ .db 192 \ .db 3, 4 \ .db 2, 2 \ .dw 4
	Wall4: .db 7 \ .db 224 \ .db 4, 5 \ .db 0, 0 \ .dw 8
	Wall5: .db 5 \ .db 0 \ .db 5, 6 \ .db 0, 0 \ .dw 1
	Wall6: .db 4 \ .db 0 \ .db 6, 7 \ .db 0, 0 \ .dw 16
	Wall7: .db 6 \ .db 0 \ .db 7, 8 \ .db 0, 0 \ .dw 1
	Wall8: .db 7 \ .db 32 \ .db 8, 9 \ .db 0, 0 \ .dw 8
	Wall9: .db 7 \ .db 64 \ .db 9, 10 \ .db 38, 38 \ .dw 4
	Wall10: .db 3 \ .db 0 \ .db 10, 11 \ .db 38, 9 \ .dw 9
	Wall11: .db 7 \ .db 192 \ .db 11, 12 \ .db 38, 38 \ .dw 4
	Wall12: .db 7 \ .db 224 \ .db 12, 13 \ .db 0, 0 \ .dw 8
	Wall13: .db 7 \ .db 192 \ .db 13, 14 \ .db 1, 1 \ .dw 100
	Wall14: .db 4 \ .db 160 \ .db 14, 15 \ .db 1, 1 \ .dw 8
	Wall15: .db 5 \ .db 128 \ .db 15, 16 \ .db 1, 1 \ .dw 36
	Wall16: .db 4 \ .db 128 \ .db 16, 17 \ .db 1, 1 \ .dw 16
	Wall17: .db 6 \ .db 128 \ .db 17, 18 \ .db 1, 1 \ .dw 36
	Wall18: .db 4 \ .db 96 \ .db 18, 19 \ .db 1, 1 \ .dw 8
	Wall19: .db 7 \ .db 64 \ .db 19, 0 \ .db 1, 1 \ .dw 100
	Wall20: .db 3 \ .db 0 \ .db 1, 4 \ .db 0, 2 \ .dw 9
	Wall21: .db 3 \ .db 128 \ .db 4, 1 \ .db 2, 0 \ .dw 9
	Wall22: .db 3 \ .db 0 \ .db 9, 12 \ .db 0, 38 \ .dw 9
	Wall23: .db 3 \ .db 128 \ .db 12, 9 \ .db 38, 0 \ .dw 9
	Wall24: .db 3 \ .db 230 \ .db 0, 20 \ .db 1, 0 \ .dw 100
	Wall25: .db 3 \ .db 26 \ .db 21, 13 \ .db 1, 0 \ .dw 100
	Wall26: .db 3 \ .db 128 \ .db 21, 20 \ .db 0, 3 \ .dw 16
	Wall27: .db 3 \ .db 128 \ .db 23, 22 \ .db 3, 4 \ .dw 16
	Wall28: .db 3 \ .db 128 \ .db 25, 24 \ .db 4, 1 \ .dw 16
	Wall29: .db 1 \ .db 64 \ .db 24, 22 \ .db 4, 1 \ .dw 4
	Wall30: .db 2 \ .db 64 \ .db 22, 20 \ .db 3, 1 \ .dw 4
	Wall31: .db 1 \ .db 192 \ .db 21, 23 \ .db 3, 1 \ .dw 4
	Wall32: .db 2 \ .db 192 \ .db 23, 25 \ .db 4, 1 \ .dw 4
	Wall33: .db 7 \ .db 0 \ .db 26, 27 \ .db 5, 5 \ .dw 64
	Wall34: .db 7 \ .db 192 \ .db 27, 28 \ .db 5, 5 \ .dw 4
	Wall35: .db 7 \ .db 64 \ .db 29, 30 \ .db 5, 5 \ .dw 4
	Wall36: .db 7 \ .db 0 \ .db 30, 31 \ .db 40, 40 \ .dw 64
	Wall37: .db 5 \ .db 192 \ .db 31, 32 \ .db 5, 5 \ .dw 25
	Wall38: .db 4 \ .db 192 \ .db 32, 33 \ .db 5, 5 \ .dw 9
	Wall39: .db 6 \ .db 192 \ .db 33, 34 \ .db 40, 40 \ .dw 1
	Wall40: .db 7 \ .db 160 \ .db 34, 35 \ .db 5, 5 \ .dw 18
	Wall41: .db 7 \ .db 128 \ .db 35, 36 \ .db 5, 5 \ .dw 1
	Wall42: .db 7 \ .db 192 \ .db 36, 37 \ .db 5, 5 \ .dw 1
	Wall43: .db 7 \ .db 192 \ .db 37, 11 \ .db 9, 9 \ .dw 1
	Wall44: .db 7 \ .db 64 \ .db 10, 38 \ .db 9, 9 \ .dw 1
	Wall45: .db 7 \ .db 64 \ .db 38, 39 \ .db 5, 5 \ .dw 1
	Wall46: .db 7 \ .db 128 \ .db 39, 40 \ .db 39, 39 \ .dw 1
	Wall47: .db 7 \ .db 160 \ .db 40, 41 \ .db 7, 7 \ .dw 8
	Wall48: .db 7 \ .db 128 \ .db 41, 42 \ .db 7, 7 \ .dw 16
	Wall49: .db 7 \ .db 96 \ .db 42, 43 \ .db 7, 7 \ .dw 8
	Wall50: .db 3 \ .db 128 \ .db 40, 43 \ .db 5, 7 \ .dw 64
	Wall51: .db 7 \ .db 128 \ .db 43, 44 \ .db 5, 5 \ .dw 1
	Wall52: .db 7 \ .db 192 \ .db 44, 45 \ .db 5, 5 \ .dw 1
	Wall53: .db 7 \ .db 192 \ .db 45, 3 \ .db 8, 8 \ .dw 1
	Wall54: .db 7 \ .db 64 \ .db 2, 46 \ .db 8, 8 \ .dw 1
	Wall55: .db 7 \ .db 64 \ .db 46, 47 \ .db 5, 5 \ .dw 1
	Wall56: .db 7 \ .db 128 \ .db 47, 48 \ .db 5, 5 \ .dw 1
	Wall57: .db 7 \ .db 96 \ .db 48, 49 \ .db 5, 5 \ .dw 18
	Wall58: .db 5 \ .db 64 \ .db 49, 50 \ .db 5, 5 \ .dw 1
	Wall59: .db 4 \ .db 64 \ .db 50, 51 \ .db 5, 5 \ .dw 9
	Wall60: .db 6 \ .db 64 \ .db 51, 26 \ .db 5, 5 \ .dw 25
	Wall61: .db 1 \ .db 0 \ .db 52, 53 \ .db 6, 5 \ .dw 36
	Wall62: .db 0 \ .db 0 \ .db 53, 54 \ .db 6, 5 \ .dw 64
	Wall63: .db 2 \ .db 0 \ .db 54, 55 \ .db 6, 5 \ .dw 36
	Wall64: .db 3 \ .db 192 \ .db 55, 56 \ .db 6, 5 \ .dw 9
	Wall65: .db 3 \ .db 160 \ .db 56, 57 \ .db 6, 5 \ .dw 8
	Wall66: .db 1 \ .db 128 \ .db 57, 58 \ .db 6, 39 \ .dw 16
	Wall67: .db 0 \ .db 128 \ .db 58, 59 \ .db 6, 5 \ .dw 64
	Wall68: .db 2 \ .db 128 \ .db 59, 60 \ .db 6, 5 \ .dw 16
	Wall69: .db 3 \ .db 96 \ .db 60, 61 \ .db 6, 5 \ .dw 8
	Wall70: .db 3 \ .db 64 \ .db 61, 52 \ .db 6, 5 \ .dw 9
	Wall71: .db 3 \ .db 0 \ .db 38, 37 \ .db 9, 5 \ .dw 9
	Wall72: .db 3 \ .db 0 \ .db 46, 45 \ .db 8, 5 \ .dw 9
	Wall73: .db 5 \ .db 64 \ .db 62, 63 \ .db 10, 10 \ .dw 4
	Wall74: .db 6 \ .db 64 \ .db 63, 64 \ .db 11, 11 \ .dw 4
	Wall75: .db 5 \ .db 128 \ .db 64, 65 \ .db 13, 13 \ .dw 4
	Wall76: .db 4 \ .db 128 \ .db 65, 66 \ .db 14, 14 \ .dw 4
	Wall77: .db 4 \ .db 128 \ .db 66, 67 \ .db 15, 15 \ .dw 4
	Wall78: .db 4 \ .db 128 \ .db 67, 68 \ .db 16, 16 \ .dw 4
	Wall79: .db 6 \ .db 128 \ .db 68, 69 \ .db 17, 17 \ .dw 16
	Wall80: .db 5 \ .db 64 \ .db 69, 70 \ .db 17, 17 \ .dw 16
	Wall81: .db 4 \ .db 64 \ .db 70, 71 \ .db 18, 18 \ .dw 4
	Wall82: .db 4 \ .db 64 \ .db 71, 72 \ .db 19, 19 \ .dw 4
	Wall83: .db 6 \ .db 64 \ .db 72, 73 \ .db 20, 20 \ .dw 16
	Wall84: .db 3 \ .db 0 \ .db 73, 74 \ .db 20, 21 \ .dw 16
	Wall85: .db 5 \ .db 192 \ .db 74, 75 \ .db 20, 20 \ .dw 16
	Wall86: .db 4 \ .db 192 \ .db 75, 76 \ .db 19, 19 \ .dw 4
	Wall87: .db 6 \ .db 192 \ .db 76, 77 \ .db 18, 18 \ .dw 4
	Wall88: .db 5 \ .db 0 \ .db 77, 78 \ .db 16, 16 \ .dw 4
	Wall89: .db 4 \ .db 0 \ .db 78, 79 \ .db 15, 15 \ .dw 4
	Wall90: .db 4 \ .db 0 \ .db 79, 80 \ .db 14, 14 \ .dw 4
	Wall91: .db 4 \ .db 0 \ .db 80, 81 \ .db 13, 13 \ .dw 4
	Wall92: .db 6 \ .db 0 \ .db 81, 82 \ .db 12, 12 \ .dw 16
	Wall93: .db 5 \ .db 192 \ .db 82, 83 \ .db 12, 12 \ .dw 16
	Wall94: .db 4 \ .db 192 \ .db 83, 84 \ .db 11, 11 \ .dw 4
	Wall95: .db 6 \ .db 192 \ .db 84, 85 \ .db 10, 10 \ .dw 4
	Wall96: .db 0 \ .db 0 \ .db 62, 85 \ .db 5, 10 \ .dw 16
	Wall97: .db 3 \ .db 0 \ .db 63, 84 \ .db 10, 11 \ .dw 16
	Wall98: .db 3 \ .db 0 \ .db 64, 83 \ .db 11, 12 \ .dw 16
	Wall99: .db 3 \ .db 64 \ .db 64, 81 \ .db 12, 13 \ .dw 16
	Wall100: .db 3 \ .db 64 \ .db 65, 80 \ .db 13, 14 \ .dw 16
	Wall101: .db 3 \ .db 64 \ .db 66, 79 \ .db 14, 15 \ .dw 16
	Wall102: .db 3 \ .db 64 \ .db 67, 78 \ .db 15, 16 \ .dw 16
	Wall103: .db 3 \ .db 64 \ .db 68, 77 \ .db 16, 17 \ .dw 16
	Wall104: .db 3 \ .db 0 \ .db 70, 77 \ .db 17, 18 \ .dw 16
	Wall105: .db 3 \ .db 0 \ .db 71, 76 \ .db 18, 19 \ .dw 16
	Wall106: .db 3 \ .db 0 \ .db 72, 75 \ .db 19, 20 \ .dw 16
	Wall107: .db 7 \ .db 0 \ .db 28, 62 \ .db 5, 5 \ .dw 9
	Wall108: .db 7 \ .db 0 \ .db 85, 29 \ .db 5, 5 \ .dw 1
	Wall109: .db 7 \ .db 64 \ .db 73, 86 \ .db 21, 21 \ .dw 16
	Wall110: .db 5 \ .db 0 \ .db 86, 87 \ .db 21, 21 \ .dw 16
	Wall111: .db 6 \ .db 0 \ .db 87, 88 \ .db 21, 21 \ .dw 4
	Wall112: .db 7 \ .db 221 \ .db 88, 89 \ .db 21, 21 \ .dw 85
	Wall113: .db 5 \ .db 0 \ .db 89, 90 \ .db 21, 21 \ .dw 25
	Wall114: .db 6 \ .db 0 \ .db 90, 91 \ .db 21, 21 \ .dw 9
	Wall115: .db 7 \ .db 32 \ .db 91, 92 \ .db 21, 21 \ .dw 18
	Wall116: .db 7 \ .db 0 \ .db 92, 93 \ .db 21, 21 \ .dw 9
	Wall117: .db 5 \ .db 192 \ .db 93, 94 \ .db 21, 21 \ .dw 9
	Wall118: .db 6 \ .db 192 \ .db 94, 95 \ .db 21, 21 \ .dw 36
	Wall119: .db 7 \ .db 0 \ .db 95, 104 \ .db 22, 22 \ .dw 1
	Wall120: .db 6 \ .db 192 \ .db 96, 97 \ .db 22, 22 \ .dw 16
	Wall121: .db 5 \ .db 128 \ .db 97, 98 \ .db 22, 22 \ .dw 25
	Wall122: .db 6 \ .db 128 \ .db 98, 99 \ .db 21, 21 \ .dw 81
	Wall123: .db 7 \ .db 64 \ .db 99, 100 \ .db 21, 21 \ .dw 36
	Wall124: .db 5 \ .db 128 \ .db 100, 101 \ .db 21, 21 \ .dw 25
	Wall125: .db 6 \ .db 128 \ .db 101, 102 \ .db 21, 21 \ .dw 16
	Wall126: .db 7 \ .db 75 \ .db 102, 103 \ .db 21, 21 \ .dw 53
	Wall127: .db 7 \ .db 128 \ .db 103, 74 \ .db 21, 21 \ .dw 4
	Wall128: .db 2 \ .db 192 \ .db 95, 98 \ .db 21, 22 \ .dw 16
	Wall129: .db 5 \ .db 64 \ .db 104, 105 \ .db 22, 22 \ .dw 4
	Wall130: .db 4 \ .db 64 \ .db 105, 106 \ .db 23, 23 \ .dw 4
	Wall131: .db 6 \ .db 64 \ .db 106, 107 \ .db 24, 24 \ .dw 25
	Wall132: .db 5 \ .db 0 \ .db 107, 108 \ .db 25, 25 \ .dw 16
	Wall133: .db 4 \ .db 0 \ .db 108, 109 \ .db 26, 26 \ .dw 4
	Wall134: .db 4 \ .db 0 \ .db 109, 110 \ .db 27, 27 \ .dw 4
	Wall135: .db 4 \ .db 0 \ .db 110, 111 \ .db 28, 28 \ .dw 4
	Wall136: .db 6 \ .db 0 \ .db 111, 112 \ .db 29, 29 \ .dw 36
	Wall137: .db 5 \ .db 192 \ .db 112, 113 \ .db 30, 30 \ .dw 25
	Wall138: .db 4 \ .db 192 \ .db 113, 114 \ .db 31, 31 \ .dw 4
	Wall139: .db 4 \ .db 192 \ .db 114, 115 \ .db 32, 32 \ .dw 4
	Wall140: .db 4 \ .db 192 \ .db 115, 127 \ .db 33, 33 \ .dw 4
	Wall141: .db 3 \ .db 128 \ .db 116, 117 \ .db 33, 34 \ .dw 9
	Wall142: .db 7 \ .db 64 \ .db 117, 118 \ .db 33, 33 \ .dw 4
	Wall143: .db 7 \ .db 128 \ .db 118, 119 \ .db 33, 33 \ .dw 9
	Wall144: .db 5 \ .db 64 \ .db 119, 120 \ .db 33, 33 \ .dw 4
	Wall145: .db 4 \ .db 64 \ .db 120, 121 \ .db 32, 32 \ .dw 4
	Wall146: .db 6 \ .db 64 \ .db 121, 122 \ .db 31, 31 \ .dw 4
	Wall147: .db 5 \ .db 128 \ .db 122, 123 \ .db 28, 28 \ .dw 4
	Wall148: .db 4 \ .db 128 \ .db 123, 124 \ .db 27, 27 \ .dw 4
	Wall149: .db 6 \ .db 128 \ .db 124, 125 \ .db 26, 26 \ .dw 4
	Wall150: .db 5 \ .db 192 \ .db 125, 126 \ .db 23, 23 \ .dw 4
	Wall151: .db 4 \ .db 192 \ .db 126, 96 \ .db 22, 22 \ .dw 4
	Wall152: .db 3 \ .db 0 \ .db 105, 126 \ .db 22, 23 \ .dw 16
	Wall153: .db 3 \ .db 0 \ .db 106, 125 \ .db 23, 24 \ .dw 16
	Wall154: .db 3 \ .db 220 \ .db 107, 125 \ .db 24, 25 \ .dw 41
	Wall155: .db 3 \ .db 192 \ .db 108, 125 \ .db 25, 26 \ .dw 25
	Wall156: .db 3 \ .db 192 \ .db 109, 124 \ .db 26, 27 \ .dw 25
	Wall157: .db 3 \ .db 192 \ .db 110, 123 \ .db 27, 28 \ .dw 25
	Wall158: .db 3 \ .db 192 \ .db 111, 122 \ .db 28, 29 \ .dw 25
	Wall159: .db 3 \ .db 157 \ .db 112, 122 \ .db 29, 30 \ .dw 61
	Wall160: .db 3 \ .db 128 \ .db 113, 122 \ .db 30, 31 \ .dw 36
	Wall161: .db 3 \ .db 128 \ .db 114, 121 \ .db 31, 32 \ .dw 36
	Wall162: .db 3 \ .db 128 \ .db 115, 120 \ .db 32, 33 \ .dw 36
	Wall163: .db 7 \ .db 192 \ .db 127, 116 \ .db 33, 33 \ .dw 4
	Wall164: .db 5 \ .db 0 \ .db 138, 139 \ .db 36, 36 \ .dw 9
	Wall165: .db 6 \ .db 0 \ .db 139, 117 \ .db 35, 35 \ .dw 9
	Wall166: .db 5 \ .db 0 \ .db 116, 128 \ .db 35, 35 \ .dw 9
	Wall167: .db 6 \ .db 0 \ .db 128, 129 \ .db 36, 36 \ .dw 9
	Wall168: .db 5 \ .db 192 \ .db 129, 130 \ .db 36, 36 \ .dw 36
	Wall169: .db 4 \ .db 192 \ .db 130, 131 \ .db 36, 36 \ .dw 9
	Wall170: .db 4 \ .db 192 \ .db 131, 132 \ .db 37, 37 \ .dw 9
	Wall171: .db 6 \ .db 192 \ .db 132, 133 \ .db 37, 37 \ .dw 49
	Wall172: .db 7 \ .db 128 \ .db 133, 158 \ .db 37, 37 \ .dw 36
	Wall173: .db 0 \ .db 64 \ .db 153, 152 \ .db 37, 43 \ .dw 9
	Wall174: .db 4 \ .db 64 \ .db 135, 136 \ .db 37, 37 \ .dw 9
	Wall175: .db 4 \ .db 64 \ .db 136, 137 \ .db 36, 36 \ .dw 9
	Wall176: .db 6 \ .db 64 \ .db 137, 138 \ .db 36, 36 \ .dw 36
	Wall177: .db 3 \ .db 192 \ .db 139, 140 \ .db 36, 35 \ .dw 36
	Wall178: .db 3 \ .db 224 \ .db 140, 141 \ .db 36, 35 \ .dw 18
	Wall179: .db 3 \ .db 0 \ .db 141, 142 \ .db 36, 35 \ .dw 9
	Wall180: .db 3 \ .db 32 \ .db 142, 143 \ .db 36, 35 \ .dw 18
	Wall181: .db 3 \ .db 64 \ .db 143, 128 \ .db 36, 35 \ .dw 36
	Wall182: .db 3 \ .db 192 \ .db 117, 144 \ .db 35, 34 \ .dw 25
	Wall183: .db 3 \ .db 224 \ .db 144, 145 \ .db 35, 34 \ .dw 2
	Wall184: .db 3 \ .db 0 \ .db 145, 146 \ .db 35, 34 \ .dw 1
	Wall185: .db 3 \ .db 32 \ .db 146, 147 \ .db 35, 34 \ .dw 2
	Wall186: .db 3 \ .db 64 \ .db 147, 116 \ .db 35, 34 \ .dw 25
	Wall187: .db 3 \ .db 224 \ .db 136, 148 \ .db 37, 36 \ .dw 18
	Wall188: .db 3 \ .db 0 \ .db 148, 149 \ .db 37, 36 \ .dw 81
	Wall189: .db 3 \ .db 32 \ .db 149, 131 \ .db 37, 36 \ .dw 18
	Wall190: .db 0 \ .db 128 \ .db 158, 157 \ .db 37, 43 \ .dw 9
	Wall191: .db 7 \ .db 128 \ .db 157, 134 \ .db 37, 37 \ .dw 36
	Wall192: .db 7 \ .db 64 \ .db 134, 153 \ .db 37, 37 \ .dw 4
	Wall193: .db 5 \ .db 64 \ .db 152, 135 \ .db 37, 37 \ .dw 4
	Wall194: .db 7 \ .db 0 \ .db 151, 152 \ .db 43, 43 \ .dw 4
	Wall195: .db 7 \ .db 128 \ .db 153, 154 \ .db 43, 43 \ .dw 4
	Wall196: .db 3 \ .db 192 \ .db 151, 154 \ .db 41, 43 \ .dw 9
	Wall197: .db 7 \ .db 192 \ .db 154, 155 \ .db 41, 41 \ .dw 16
	Wall198: .db 7 \ .db 0 \ .db 155, 156 \ .db 41, 41 \ .dw 64
	Wall199: .db 7 \ .db 64 \ .db 156, 157 \ .db 43, 43 \ .dw 4
	Wall200: .db 7 \ .db 192 \ .db 158, 159 \ .db 43, 43 \ .dw 4
	Wall201: .db 3 \ .db 0 \ .db 156, 159 \ .db 41, 43 \ .dw 9
	Wall202: .db 7 \ .db 0 \ .db 159, 160 \ .db 41, 41 \ .dw 49
	Wall203: .db 7 \ .db 32 \ .db 160, 161 \ .db 41, 41 \ .dw 8
	Wall204: .db 7 \ .db 0 \ .db 161, 162 \ .db 41, 41 \ .dw 4
	Wall205: .db 5 \ .db 64 \ .db 162, 163 \ .db 41, 41 \ .dw 1
	Wall206: .db 3 \ .db 0 \ .db 163, 164 \ .db 41, 44 \ .dw 9
	Wall207: .db 6 \ .db 192 \ .db 164, 165 \ .db 41, 41 \ .dw 1
	Wall208: .db 7 \ .db 0 \ .db 165, 166 \ .db 41, 41 \ .dw 4
	Wall209: .db 7 \ .db 224 \ .db 166, 167 \ .db 41, 41 \ .dw 8
	Wall210: .db 7 \ .db 192 \ .db 167, 168 \ .db 41, 41 \ .dw 25
	Wall211: .db 7 \ .db 160 \ .db 168, 169 \ .db 41, 41 \ .dw 8
	Wall212: .db 5 \ .db 128 \ .db 169, 170 \ .db 41, 41 \ .dw 4
	Wall213: .db 4 \ .db 128 \ .db 170, 171 \ .db 41, 41 \ .dw 9
	Wall214: .db 6 \ .db 128 \ .db 171, 172 \ .db 41, 41 \ .dw 4
	Wall215: .db 7 \ .db 96 \ .db 172, 173 \ .db 41, 41 \ .dw 8
	Wall216: .db 5 \ .db 128 \ .db 173, 181 \ .db 41, 41 \ .dw 81
	Wall217: .db 6 \ .db 128 \ .db 174, 175 \ .db 41, 41 \ .dw 25
	Wall218: .db 7 \ .db 64 \ .db 175, 176 \ .db 41, 41 \ .dw 196
	Wall219: .db 7 \ .db 0 \ .db 176, 150 \ .db 41, 41 \ .dw 25
	Wall220: .db 7 \ .db 192 \ .db 150, 151 \ .db 41, 41 \ .dw 4
	Wall221: .db 3 \ .db 0 \ .db 177, 178 \ .db 42, 41 \ .dw 9
	Wall222: .db 3 \ .db 192 \ .db 178, 179 \ .db 42, 41 \ .dw 9
	Wall223: .db 3 \ .db 128 \ .db 179, 180 \ .db 42, 41 \ .dw 9
	Wall224: .db 3 \ .db 64 \ .db 180, 177 \ .db 42, 41 \ .dw 9
	Wall225: .db 4 \ .db 128 \ .db 181, 174 \ .db 41, 41 \ .dw 81
	Wall226: .db 4 \ .db 64 \ .db 163, 182 \ .db 44, 44 \ .dw 1
	Wall227: .db 4 \ .db 192 \ .db 201, 164 \ .db 44, 44 \ .dw 1
	Wall228: .db 6 \ .db 64 \ .db 182, 183 \ .db 45, 45 \ .dw 1
	Wall229: .db 5 \ .db 192 \ .db 200, 201 \ .db 45, 45 \ .dw 1
	Wall230: .db 7 \ .db 128 \ .db 183, 184 \ .db 45, 45 \ .dw 16
	Wall231: .db 5 \ .db 64 \ .db 184, 185 \ .db 45, 45 \ .dw 64
	Wall232: .db 6 \ .db 64 \ .db 185, 186 \ .db 45, 45 \ .dw 64
	Wall233: .db 5 \ .db 0 \ .db 186, 187 \ .db 45, 45 \ .dw 36
	Wall234: .db 4 \ .db 0 \ .db 187, 188 \ .db 45, 45 \ .dw 25
	Wall235: .db 4 \ .db 0 \ .db 188, 189 \ .db 45, 45 \ .dw 4
	Wall236: .db 6 \ .db 0 \ .db 189, 190 \ .db 45, 45 \ .dw 9
	Wall237: .db 7 \ .db 224 \ .db 190, 191 \ .db 45, 45 \ .dw 8
	Wall238: .db 7 \ .db 192 \ .db 191, 192 \ .db 45, 45 \ .dw 4
	Wall239: .db 7 \ .db 169 \ .db 192, 193 \ .db 45, 45 \ .dw 13
	Wall240: .db 7 \ .db 192 \ .db 193, 194 \ .db 46, 46 \ .dw 4
	Wall241: .db 7 \ .db 216 \ .db 194, 195 \ .db 45, 45 \ .dw 13
	Wall242: .db 7 \ .db 192 \ .db 195, 196 \ .db 45, 45 \ .dw 4
	Wall243: .db 7 \ .db 160 \ .db 196, 197 \ .db 45, 45 \ .dw 8
	Wall244: .db 5 \ .db 128 \ .db 197, 198 \ .db 45, 45 \ .dw 9
	Wall245: .db 4 \ .db 128 \ .db 198, 199 \ .db 45, 45 \ .dw 4
	Wall246: .db 6 \ .db 128 \ .db 199, 200 \ .db 45, 45 \ .dw 16
	Wall247: .db 7 \ .db 128 \ .db 203, 202 \ .db 45, 45 \ .dw 25
	Wall248: .db 7 \ .db 109 \ .db 204, 203 \ .db 45, 45 \ .dw 5
	Wall249: .db 3 \ .db 0 \ .db 204, 193 \ .db 46, 45 \ .dw 9
	Wall250: .db 3 \ .db 128 \ .db 194, 205 \ .db 46, 45 \ .dw 9
	Wall251: .db 7 \ .db 18 \ .db 206, 205 \ .db 45, 45 \ .dw 5
	Wall252: .db 7 \ .db 0 \ .db 207, 206 \ .db 45, 45 \ .dw 25
	Wall253: .db 7 \ .db 192 \ .db 202, 207 \ .db 45, 45 \ .dw 16
	Wall254: .db 3 \ .db 0 \ .db 182, 201 \ .db 44, 45 \ .dw 9
	Wall255: .db 7 \ .db 64 \ .db 205, 204 \ .db 46, 46 \ .dw 4
Walls.Count = ($ - Walls) / 8

Sectors:
Sector0: .dw -128, 256
Sector1: .dw 0, 256
Sector2: .dw -128, 64
Sector3: .dw -86, 256
Sector4: .dw -42, 256
Sector5: .dw -128, 64
Sector6: .dw -160, 64
Sector7: .dw -96, 64
Sector8: .dw -128, -128
Sector9: .dw -128, -128
Sector10: .dw -128, 24
Sector11: .dw -152, 24
Sector12: .dw -176, 24
Sector13: .dw -200, 24
Sector14: .dw -224, 24
Sector15: .dw -248, 24
Sector16: .dw -272, 24
Sector17: .dw -296, 24
Sector18: .dw -320, 24
Sector19: .dw -344, 24
Sector20: .dw -368, 24
Sector21: .dw -368, -112
Sector22: .dw -368, -176
Sector23: .dw -344, -80
Sector24: .dw -320, -80
Sector25: .dw -296, -80
Sector26: .dw -272, 16
Sector27: .dw -248, 16
Sector28: .dw -224, 16
Sector29: .dw -200, 16
Sector30: .dw -176, 16
Sector31: .dw -152, 112
Sector32: .dw -128, 112
Sector33: .dw -104, 112
Sector34: .dw -104, 256
Sector35: .dw -320, 256
Sector36: .dw -352, 256
Sector37: .dw -384, 256
Sector38: .dw -128, 64
Sector39: .dw -128, 64
Sector40: .dw -128, 64
Sector41: .dw -320, -128
Sector42: .dw -320, -96
Sector43: .dw -352, -128
Sector44: .dw -352, -112
Sector45: .dw -384, -96
Sector46: .dw -334, -256

Things = $-8
Thing0: .db 0, Thing.Stand, 0, 0 \ .dw 1024, -1280
Thing1: .db 0, Thing.Stand, 0, 0 \ .dw 2560, -4352
Thing2: .db 0, Thing.Keycard, 0, 0 \ .dw 2816, -768
Thing3: .db 0, Thing.Lamp, 0, 0 \ .dw 640, -5760
Thing4: .db 0, Thing.Lamp, 0, 0 \ .dw 4480, -5760
Thing5: .db 0, Thing.Stand, 0, 0 \ .dw 5376, -10880
Thing6: .db 0, Thing.Lamp, 0, 0 \ .dw -512, -8448
Thing7: .db 0, Thing.Lamp, 0, 0 \ .dw 8960, -8704
Thing8: .db 0, Thing.MaxCoderz, 0, 0 \ .dw 9856, -6784
Thing9: .db 11, Thing.MaxCoderz, 0, 0 \ .dw 15744, -6528
Thing10: .db 0, Thing.Lamp, 0, 0 \ .dw 16128, -7168
Thing11: .db 0, Thing.Lamp, 0, 0 \ .dw 16128, -4608
Thing12: .db 0, Thing.Stand, 0, 0 \ .dw 13440, -1920
Thing13: .db 15, Thing.Lamp, 0, 0 \ .dw 6400, -4608
Thing14: .db 0, Thing.Lamp, 0, 0 \ .dw 6400, -1536

Tree:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F
.dw Tree.B
.dw 5888

Tree.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F
.dw Tree.F.B
.dw -7936

Tree.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F
.dw Tree.F.F.B
.dw 12032

Tree.F.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.F
.dw Tree.F.F.F.B
.dw -3328

Tree.F.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.F.F
.dw Tree.F.F.F.F.B
.dw 13056

Tree.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.F.F.F
.dw Tree.F.F.F.F.F.B
.dw 13824

Tree.F.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 41
	.db 0
	.db 6 \ .db 222, 208, 209, 210, 211, 212

Tree.F.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.F.F.B.F
.dw Tree.F.F.F.F.F.B.B
.dw -2304

Tree.F.F.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.F.F.B.F.F
.dw Tree.F.F.F.F.F.B.F.B
.dw -1536

Tree.F.F.F.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 41
	.db 0
	.db 2 \ .db 223, 213

Tree.F.F.F.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 42
	.db 13
	.db 4 \ .db 221, 222, 223, 224

Tree.F.F.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.F.F.B.B.F
.dw Tree.F.F.F.F.F.B.B.B
.dw -3072

Tree.F.F.F.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 41
	.db 0
	.db 1 \ .db 221

Tree.F.F.F.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 41
	.db 0
	.db 3 \ .db 206, 205, 207

Tree.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 41
	.db 0
	.db 5 \ .db 224, 203, 204, 215, 214

Tree.F.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.B.F
.dw Tree.F.F.F.B.B
.dw -3840

Tree.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.B.F.F
.dw Tree.F.F.F.B.F.B
.dw -3584

Tree.F.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 44
	.db 0
	.db 4 \ .db 206, 254, 226, 227

Tree.F.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 0
	.db 3 \ .db 254, 228, 229

Tree.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.B.B.F
.dw Tree.F.F.F.B.B.B
.dw 15360

Tree.F.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.B.B.F.F
.dw Tree.F.F.F.B.B.F.B
.dw -6144

Tree.F.F.F.B.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.B.B.F.F.F
.dw Tree.F.F.F.B.B.F.F.B
.dw -5632

Tree.F.F.F.B.B.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.B.B.F.F.F.F
.dw Tree.F.F.F.B.B.F.F.F.B
.dw 16128

Tree.F.F.F.B.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 0
	.db 3 \ .db 241, 242, 243

Tree.F.F.F.B.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 12
	.db 2 \ .db 244, 250

Tree.F.F.F.B.B.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 46
	.db 0
	.db 4 \ .db 249, 250, 255, 240

Tree.F.F.F.B.B.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.B.B.F.B.F
.dw Tree.F.F.F.B.B.F.B.B
.dw 16128

Tree.F.F.F.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 0
	.db 3 \ .db 237, 238, 239

Tree.F.F.F.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 10
	.db 2 \ .db 249, 236

Tree.F.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.B.B.B.F
.dw Tree.F.F.F.B.B.B.B
.dw 13568

Tree.F.F.F.B.B.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.B.B.B.F.F
.dw Tree.F.F.F.B.B.B.F.B
.dw 14848

Tree.F.F.F.B.B.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.B.B.B.F.F.F
.dw Tree.F.F.F.B.B.B.F.F.B
.dw -5888

Tree.F.F.F.B.B.B.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 0
	.db 2 \ .db 251, 245

Tree.F.F.F.B.B.B.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 0
	.db 2 \ .db 235, 248

Tree.F.F.F.B.B.B.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.B.B.B.F.B.F
.dw Tree.F.F.F.B.B.B.F.B.B
.dw -5888

Tree.F.F.F.B.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 0
	.db 2 \ .db 252, 246

Tree.F.F.F.B.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 0
	.db 2 \ .db 234, 247

Tree.F.F.F.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 45
	.db 0
	.db 5 \ .db 253, 230, 231, 232, 233

Tree.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.F
.dw Tree.F.F.B.B
.dw -5632

Tree.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.F.F
.dw Tree.F.F.B.F.B
.dw -4864

Tree.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.B.F.F.F
.dw Tree.F.F.B.F.F.B
.dw 7424

Tree.F.F.B.F.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.F.F.F.F
.dw Tree.F.F.B.F.F.F.B
.dw -3072

Tree.F.F.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.F.F.F.F.F
.dw Tree.F.F.B.F.F.F.F.B
.dw -2560

Tree.F.F.B.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 41
	.db 0
	.db 5 \ .db 201, 198, 202, 216, 225

Tree.F.F.B.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 43
	.db 0
	.db 4 \ .db 190, 201, 199, 200

Tree.F.F.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.B.F.F.F.B.F
.dw Tree.F.F.B.F.F.F.B.B
.dw 7936

Tree.F.F.B.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 37
	.db 0
	.db 8 \ .db 188, 190, 173, 171, 172, 191, 192, 193

Tree.F.F.B.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 43
	.db 0
	.db 4 \ .db 173, 196, 194, 195

Tree.F.F.B.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 41
	.db 14
	.db 6 \ .db 196, 219, 218, 217, 197, 220

Tree.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.B.F.B.F
.dw Tree.F.F.B.F.B.B
.dw -13568
.dw 256

Tree.F.F.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 37
	.db 0
	.db 2 \ .db 187, 174

Tree.F.F.B.F.B.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.B.F.B.B.F
.dw Tree.F.F.B.F.B.B.B
.dw 6144
.dw -256

Tree.F.F.B.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 37
	.db 0
	.db 2 \ .db 170, 189

Tree.F.F.B.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 36
	.db 0
	.db 4 \ .db 187, 179, 188, 189

Tree.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.B.F
.dw Tree.F.F.B.B.B
.dw -6400

Tree.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.B.B.F.F
.dw Tree.F.F.B.B.F.B
.dw -15104
.dw 256

Tree.F.F.B.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 36
	.db 0
	.db 2 \ .db 178, 175

Tree.F.F.B.B.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.B.B.F.B.F
.dw Tree.F.F.B.B.F.B.B
.dw 4608
.dw -256

Tree.F.F.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 36
	.db 0
	.db 2 \ .db 180, 169

Tree.F.F.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 35
	.db 0
	.db 4 \ .db 178, 179, 184, 180

Tree.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.B.B.B.F
.dw Tree.F.F.B.B.B.B
.dw 9472

Tree.F.F.B.B.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.B.B.B.F.F
.dw Tree.F.F.B.B.B.F.B
.dw 10240

Tree.F.F.B.B.B.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.B.B.B.F.F.F
.dw Tree.F.F.B.B.B.F.F.B
.dw 11008

Tree.F.F.B.B.B.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 36
	.db 0
	.db 3 \ .db 181, 167, 168

Tree.F.F.B.B.B.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 35
	.db 0
	.db 4 \ .db 186, 185, 181, 166

Tree.F.F.B.B.B.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.B.B.B.F.B.F
.dw Tree.F.F.B.B.B.F.B.B
.dw -16128
.dw 256

Tree.F.F.B.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 35
	.db 0
	.db 1 \ .db 183

Tree.F.F.B.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.B.B.B.F.B.B.F
.dw Tree.F.F.B.B.B.F.B.B.B
.dw 3584
.dw -256

Tree.F.F.B.B.B.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 35
	.db 0
	.db 1 \ .db 185

Tree.F.F.B.B.B.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 34
	.db 9
	.db 6 \ .db 141, 182, 183, 184, 185, 186

Tree.F.F.B.B.B.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.B.B.B.B.F
.dw Tree.F.F.B.B.B.B.B
.dw 8704

Tree.F.F.B.B.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 35
	.db 0
	.db 3 \ .db 177, 182, 165

Tree.F.F.B.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 36
	.db 0
	.db 3 \ .db 177, 176, 164

Tree.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.F
.dw Tree.F.B.B
.dw 7168

Tree.F.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.F.F
.dw Tree.F.B.F.B
.dw 8704

Tree.F.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F.F
.dw Tree.F.B.F.F.B
.dw -9472

Tree.F.B.F.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F.F.F
.dw Tree.F.B.F.F.F.B
.dw -8960

Tree.F.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F.F.F.F
.dw Tree.F.B.F.F.F.F.B
.dw -8448

Tree.F.B.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 33
	.db 0
	.db 3 \ .db 141, 142, 163

Tree.F.B.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 33
	.db 8
	.db 4 \ .db 162, 144, 143, 140

Tree.F.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 32
	.db 0
	.db 4 \ .db 161, 162, 139, 145

Tree.F.B.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F.B.F
.dw Tree.F.B.F.F.B.B
.dw -9984

Tree.F.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 31
	.db 0
	.db 4 \ .db 160, 161, 138, 146

Tree.F.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.B.F.F.B.B.F
.dw Tree.F.B.F.F.B.B.B
.dw -2744
.dw -213

Tree.F.B.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 30
	.db 0
	.db 3 \ .db 159, 160, 137

Tree.F.B.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 29
	.db 0
	.db 3 \ .db 158, 159, 136

Tree.F.B.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.F.B.F
.dw Tree.F.B.F.B.B
.dw 8192

Tree.F.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 28
	.db 0
	.db 4 \ .db 157, 158, 135, 147

Tree.F.B.F.B.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.F.B.B.F
.dw Tree.F.B.F.B.B.B
.dw 7680

Tree.F.B.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 27
	.db 0
	.db 4 \ .db 156, 157, 134, 148

Tree.F.B.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 26
	.db 0
	.db 4 \ .db 155, 156, 133, 149

Tree.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.B.F
.dw Tree.F.B.B.B
.dw -9472

Tree.F.B.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.B.F.F
.dw Tree.F.B.B.F.B
.dw -8960

Tree.F.B.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 22
	.db 0
	.db 4 \ .db 128, 119, 120, 121

Tree.F.B.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 22
	.db 0
	.db 3 \ .db 152, 129, 151

Tree.F.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.B.B.F
.dw Tree.F.B.B.B.B
.dw -9984

Tree.F.B.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 23
	.db 0
	.db 4 \ .db 152, 153, 130, 150

Tree.F.B.B.B.B:
.db Nostromo.Tree.Node.Type.SteepSlopePartition
.dw Tree.F.B.B.B.B.F
.dw Tree.F.B.B.B.B.B
.dw 15120
.dw 204

Tree.F.B.B.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 25
	.db 0
	.db 3 \ .db 154, 155, 132

Tree.F.B.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 24
	.db 0
	.db 3 \ .db 153, 154, 131

Tree.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F
.dw Tree.B.B
.dw -7680

Tree.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F
.dw Tree.B.F.B
.dw -4096

Tree.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.F
.dw Tree.B.F.F.B
.dw -3072

Tree.B.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.F.F.F
.dw Tree.B.F.F.F.B
.dw 2048

Tree.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.F.F.F.F
.dw Tree.B.F.F.F.F.B
.dw 3072

Tree.B.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.B.F.F.F.F.F.F
.dw Tree.B.F.F.F.F.F.B
.dw 768
.dw -192

Tree.B.F.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 1
	.db 0
	.db 6 \ .db 13, 31, 32, 15, 25, 14

Tree.B.F.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 0
	.db 0
	.db 1 \ .db 25

Tree.B.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.F.F.B.F
.dw Tree.B.F.F.F.F.B.B
.dw -1024

Tree.B.F.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.F.F.B.F.F
.dw Tree.B.F.F.F.F.B.F.B
.dw -512

Tree.B.F.F.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 1
	.db 0
	.db 2 \ .db 28, 16

Tree.B.F.F.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 4
	.db 3
	.db 4 \ .db 27, 28, 29, 32

Tree.B.F.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.F.F.B.B.F
.dw Tree.B.F.F.F.F.B.B.B
.dw -1536

Tree.B.F.F.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 3
	.db 0
	.db 4 \ .db 26, 27, 30, 31

Tree.B.F.F.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 0
	.db 0
	.db 2 \ .db 26, 6

Tree.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.B.F.F.F.B.F
.dw Tree.B.F.F.F.B.B
.dw -3072
.dw 192

Tree.B.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 1
	.db 1
	.db 6 \ .db 29, 30, 19, 17, 24, 18

Tree.B.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 0
	.db 0
	.db 1 \ .db 24

Tree.B.F.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.F.B.F
.dw Tree.B.F.F.B.B
.dw 2560

Tree.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.B.F.F
.dw Tree.B.F.F.B.F.B
.dw -3584

Tree.B.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 0
	.db 0
	.db 4 \ .db 7, 8, 12, 22

Tree.B.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 38
	.db 0
	.db 4 \ .db 9, 10, 11, 23

Tree.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.B.B.F
.dw Tree.B.F.F.B.B.B
.dw -3584

Tree.B.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 0
	.db 0
	.db 4 \ .db 5, 0, 4, 20

Tree.B.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 2
	.db 0
	.db 4 \ .db 1, 2, 3, 21

Tree.B.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.B.F
.dw Tree.B.F.B.B
.dw 1536

Tree.B.F.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.B.F.F
.dw Tree.B.F.B.F.B
.dw 3584

Tree.B.F.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.F.F
.dw Tree.B.F.B.F.F.B
.dw -5632

Tree.B.F.B.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.B.F.F.F.F
.dw Tree.B.F.B.F.F.F.B
.dw 4608

Tree.B.F.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.B.F.B.F.F.F.F.F
.dw Tree.B.F.B.F.F.F.F.B
.dw -512
.dw -256

Tree.B.F.B.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 4 \ .db 65, 39, 40, 41

Tree.B.F.B.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 6
	.db 0
	.db 1 \ .db 65

Tree.B.F.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.F.F.B.F
.dw Tree.B.F.B.F.F.F.B.B
.dw -4608

Tree.B.F.B.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.F.F.B.F.F
.dw Tree.B.F.B.F.F.F.B.F.B
.dw -4352

Tree.B.F.B.F.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 9
	.db 0
	.db 4 \ .db 10, 43, 44, 71

Tree.B.F.B.F.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 3 \ .db 71, 42, 45

Tree.B.F.B.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.F.F.B.B.F
.dw Tree.B.F.B.F.F.F.B.B.B
.dw -5120

Tree.B.F.B.F.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 39
	.db 0
	.db 2 \ .db 66, 46

Tree.B.F.B.F.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 6
	.db 0
	.db 1 \ .db 66

Tree.B.F.B.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.F.B.F
.dw Tree.B.F.B.F.F.B.B
.dw -6400

Tree.B.F.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.B.F.F.B.F.F
.dw Tree.B.F.B.F.F.B.F.B
.dw 5120

Tree.B.F.B.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 2 \ .db 64, 38

Tree.B.F.B.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 6
	.db 5
	.db 2 \ .db 63, 64

Tree.B.F.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 4 \ .db 63, 35, 36, 37

Tree.B.F.B.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.B.F
.dw Tree.B.F.B.F.B.B
.dw -5120

Tree.B.F.B.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.B.F.F
.dw Tree.B.F.B.F.B.F.B
.dw -4608

Tree.B.F.B.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 7
	.db 2
	.db 4 \ .db 50, 47, 48, 49

Tree.B.F.B.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 2 \ .db 67, 50

Tree.B.F.B.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.B.B.F
.dw Tree.B.F.B.F.B.B.B
.dw -6400

Tree.B.F.B.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 6
	.db 0
	.db 2 \ .db 62, 67

Tree.B.F.B.F.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F.B.B.B.F
.dw Tree.B.F.B.F.B.B.B.B
.dw -7168

Tree.B.F.B.F.B.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 4 \ .db 62, 96, 107, 108

Tree.B.F.B.F.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 10
	.db 0
	.db 4 \ .db 96, 97, 73, 95

Tree.B.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.B.F
.dw Tree.B.F.B.B.B
.dw -5632

Tree.B.F.B.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.B.B.F.F
.dw Tree.B.F.B.B.F.B
.dw 512

Tree.B.F.B.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.B.F.F.F
.dw Tree.B.F.B.B.F.F.B
.dw -4608

Tree.B.F.B.B.F.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.B.F.F.F.F
.dw Tree.B.F.B.B.F.F.F.B
.dw -4352

Tree.B.F.B.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 8
	.db 0
	.db 4 \ .db 2, 72, 53, 54

Tree.B.F.B.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 3 \ .db 72, 52, 55

Tree.B.F.B.B.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.B.F.F.B.F
.dw Tree.B.F.B.B.F.F.B.B
.dw -5120

Tree.B.F.B.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 40
	.db 0
	.db 2 \ .db 68, 51

Tree.B.F.B.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 6
	.db 0
	.db 1 \ .db 68

Tree.B.F.B.B.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.B.F.B.B.F.B.F
.dw Tree.B.F.B.B.F.B.B
.dw -5632
.dw 256

Tree.B.F.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 4 \ .db 56, 57, 58, 69

Tree.B.F.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 6
	.db 0
	.db 1 \ .db 69

Tree.B.F.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.B.B.F
.dw Tree.B.F.B.B.B.B
.dw -6400

Tree.B.F.B.B.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.B.B.B.F.F
.dw Tree.B.F.B.B.B.F.B
.dw 0

Tree.B.F.B.B.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 6
	.db 4
	.db 2 \ .db 61, 70

Tree.B.F.B.B.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 2 \ .db 59, 70

Tree.B.F.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 5
	.db 0
	.db 4 \ .db 61, 33, 34, 60

Tree.B.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.F
.dw Tree.B.B.B
.dw 2304

Tree.B.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.F.F
.dw Tree.B.B.F.B
.dw 3584

Tree.B.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.F.F.F
.dw Tree.B.B.F.F.B
.dw -10496

Tree.B.B.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 21
	.db 0
	.db 5 \ .db 128, 114, 118, 122, 123

Tree.B.B.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 21
	.db 6
	.db 3 \ .db 115, 116, 117

Tree.B.B.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.F.B.F
.dw Tree.B.B.F.B.B
.dw -8192

Tree.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 11
	.db 0
	.db 4 \ .db 97, 98, 74, 94

Tree.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.F.B.B.F
.dw Tree.B.B.F.B.B.B
.dw -9344

Tree.B.B.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 12
	.db 0
	.db 4 \ .db 98, 99, 92, 93

Tree.B.B.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 21
	.db 0
	.db 2 \ .db 113, 124

Tree.B.B.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.B.F
.dw Tree.B.B.B.B
.dw 256

Tree.B.B.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.F.F
.dw Tree.B.B.B.F.B
.dw -9344

Tree.B.B.B.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.B.F.F.F
.dw Tree.B.B.B.F.F.B
.dw 1280

Tree.B.B.B.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.B.F.F.F.F
.dw Tree.B.B.B.F.F.F.B
.dw 1792

Tree.B.B.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 13
	.db 0
	.db 4 \ .db 99, 100, 75, 91

Tree.B.B.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 14
	.db 0
	.db 4 \ .db 100, 101, 76, 90

Tree.B.B.B.F.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.B.F.F.B.F
.dw Tree.B.B.B.F.F.B.B
.dw 768

Tree.B.B.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 15
	.db 0
	.db 4 \ .db 101, 102, 77, 89

Tree.B.B.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 16
	.db 0
	.db 4 \ .db 102, 103, 78, 88

Tree.B.B.B.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.B.F.B.F
.dw Tree.B.B.B.F.B.B
.dw 768

Tree.B.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 21
	.db 0
	.db 3 \ .db 112, 126, 125

Tree.B.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 21
	.db 0
	.db 2 \ .db 111, 127

Tree.B.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.B.F
.dw Tree.B.B.B.B.B
.dw -9728

Tree.B.B.B.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.B.F.F
.dw Tree.B.B.B.B.F.B
.dw -9216

Tree.B.B.B.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 17
	.db 7
	.db 4 \ .db 103, 104, 79, 80

Tree.B.B.B.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 18
	.db 0
	.db 4 \ .db 104, 105, 81, 87

Tree.B.B.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.B.B.F
.dw Tree.B.B.B.B.B.B
.dw -10240

Tree.B.B.B.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 19
	.db 0
	.db 4 \ .db 105, 106, 82, 86

Tree.B.B.B.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.B.B.B.F
.dw Tree.B.B.B.B.B.B.B
.dw -11264

Tree.B.B.B.B.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
	.db 20
	.db 0
	.db 4 \ .db 106, 84, 83, 85

Tree.B.B.B.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
	.db 21
	.db 0
	.db 3 \ .db 84, 109, 110

BlockMap:
.dw -1280, -12800 \ .db 18, 13
.dw BlockMapList0, BlockMapList1, BlockMapList2, BlockMapList3, BlockMapList4, BlockMapList5, BlockMapList6, BlockMapList7, BlockMapList8, BlockMapList9, BlockMapList10, BlockMapList11, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12
.dw BlockMapList13, BlockMapList14, BlockMapList15, BlockMapList16, BlockMapList17, BlockMapList18, BlockMapList19, BlockMapList20, BlockMapList21, BlockMapList22, BlockMapList23, BlockMapList10, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12
.dw BlockMapList24, BlockMapList25, BlockMapList26, BlockMapList27, BlockMapList28, BlockMapList29, BlockMapList30, BlockMapList31, BlockMapList32, BlockMapList33, BlockMapList34, BlockMapList35, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12
.dw BlockMapList36, BlockMapList37, BlockMapList38, BlockMapList39, BlockMapList40, BlockMapList41, BlockMapList42, BlockMapList43, BlockMapList44, BlockMapList45, BlockMapList46, BlockMapList47, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12
.dw BlockMapList48, BlockMapList49, BlockMapList50, BlockMapList51, BlockMapList52, BlockMapList53, BlockMapList54, BlockMapList55, BlockMapList56, BlockMapList57, BlockMapList58, BlockMapList59, BlockMapList60, BlockMapList61, BlockMapList62, BlockMapList63, BlockMapList64, BlockMapList65
.dw BlockMapList66, BlockMapList67, BlockMapList68, BlockMapList69, BlockMapList70, BlockMapList71, BlockMapList72, BlockMapList73, BlockMapList74, BlockMapList75, BlockMapList76, BlockMapList77, BlockMapList78, BlockMapList79, BlockMapList80, BlockMapList81, BlockMapList82, BlockMapList83
.dw BlockMapList84, BlockMapList84, BlockMapList85, BlockMapList86, BlockMapList87, BlockMapList88, BlockMapList89, BlockMapList89, BlockMapList90, BlockMapList91, BlockMapList92, BlockMapList92, BlockMapList93, BlockMapList94, BlockMapList95, BlockMapList96, BlockMapList97, BlockMapList98
.dw BlockMapList99, BlockMapList100, BlockMapList101, BlockMapList102, BlockMapList103, BlockMapList104, BlockMapList105, BlockMapList106, BlockMapList107, BlockMapList107, BlockMapList12, BlockMapList12, BlockMapList108, BlockMapList109, BlockMapList110, BlockMapList111, BlockMapList112, BlockMapList113
.dw BlockMapList114, BlockMapList115, BlockMapList116, BlockMapList117, BlockMapList118, BlockMapList119, BlockMapList120, BlockMapList121, BlockMapList122, BlockMapList123, BlockMapList124, BlockMapList124, BlockMapList125, BlockMapList126, BlockMapList127, BlockMapList128, BlockMapList129, BlockMapList130
.dw BlockMapList131, BlockMapList132, BlockMapList133, BlockMapList134, BlockMapList135, BlockMapList136, BlockMapList137, BlockMapList138, BlockMapList139, BlockMapList140, BlockMapList141, BlockMapList141, BlockMapList142, BlockMapList143, BlockMapList127, BlockMapList144, BlockMapList145, BlockMapList146
.dw BlockMapList131, BlockMapList147, BlockMapList148, BlockMapList149, BlockMapList150, BlockMapList151, BlockMapList152, BlockMapList153, BlockMapList154, BlockMapList155, BlockMapList156, BlockMapList156, BlockMapList157, BlockMapList158, BlockMapList159, BlockMapList160, BlockMapList161, BlockMapList12
.dw BlockMapList162, BlockMapList163, BlockMapList164, BlockMapList165, BlockMapList166, BlockMapList167, BlockMapList168, BlockMapList169, BlockMapList170, BlockMapList170, BlockMapList171, BlockMapList171, BlockMapList172, BlockMapList173, BlockMapList174, BlockMapList175, BlockMapList176, BlockMapList12
.dw BlockMapList177, BlockMapList178, BlockMapList179, BlockMapList180, BlockMapList181, BlockMapList182, BlockMapList182, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList12, BlockMapList183, BlockMapList184, BlockMapList174, BlockMapList185, BlockMapList186, BlockMapList12
BlockMapList0: .db 6 \ .db 83, 85, 109, 110, 111, 127
BlockMapList1: .db 8 \ .db 83, 85, 109, 110, 111, 112, 126, 127
BlockMapList2: .db 6 \ .db 85, 110, 111, 112, 126, 127
BlockMapList3: .db 2 \ .db 112, 126
BlockMapList4: .db 2 \ .db 112, 115
BlockMapList5: .db 2 \ .db 115, 116
BlockMapList6: .db 5 \ .db 115, 116, 117, 131, 132
BlockMapList7: .db 4 \ .db 117, 131, 132, 133
BlockMapList8: .db 3 \ .db 133, 134, 135
BlockMapList9: .db 3 \ .db 134, 135, 136
BlockMapList10: .db 2 \ .db 136, 137
BlockMapList11: .db 1 \ .db 137
BlockMapList12: .db 0
BlockMapList13: .db 8 \ .db 82, 83, 85, 86, 109, 110, 111, 127
BlockMapList14: .db 10 \ .db 82, 83, 85, 86, 109, 110, 111, 112, 126, 127
BlockMapList15: .db 8 \ .db 85, 86, 110, 111, 112, 113, 126, 127
BlockMapList16: .db 3 \ .db 112, 113, 126
BlockMapList17: .db 4 \ .db 112, 113, 114, 115
BlockMapList18: .db 4 \ .db 113, 114, 115, 116
BlockMapList19: .db 7 \ .db 114, 115, 116, 117, 118, 131, 132
BlockMapList20: .db 6 \ .db 116, 117, 118, 131, 132, 133
BlockMapList21: .db 4 \ .db 132, 133, 134, 135
BlockMapList22: .db 4 \ .db 133, 134, 135, 136
BlockMapList23: .db 3 \ .db 135, 136, 137
BlockMapList24: .db 10 \ .db 80, 81, 82, 83, 85, 86, 87, 88, 109, 127
BlockMapList25: .db 15 \ .db 80, 81, 82, 83, 85, 86, 87, 88, 89, 90, 109, 112, 125, 126, 127
BlockMapList26: .db 14 \ .db 85, 86, 87, 88, 89, 90, 91, 92, 112, 113, 124, 125, 126, 127
BlockMapList27: .db 9 \ .db 90, 91, 92, 93, 112, 113, 124, 125, 126
BlockMapList28: .db 9 \ .db 92, 93, 112, 113, 114, 115, 123, 124, 125
BlockMapList29: .db 7 \ .db 93, 113, 114, 115, 116, 123, 124
BlockMapList30: .db 9 \ .db 114, 115, 116, 117, 118, 129, 130, 131, 132
BlockMapList31: .db 11 \ .db 116, 117, 118, 129, 130, 131, 132, 133, 149, 150, 151
BlockMapList32: .db 9 \ .db 132, 133, 134, 135, 147, 148, 149, 150, 151
BlockMapList33: .db 9 \ .db 133, 134, 135, 136, 145, 146, 147, 148, 149
BlockMapList34: .db 8 \ .db 135, 136, 137, 138, 139, 145, 146, 147
BlockMapList35: .db 4 \ .db 136, 137, 138, 139
BlockMapList36: .db 10 \ .db 78, 79, 80, 81, 82, 83, 85, 86, 87, 88
BlockMapList37: .db 16 \ .db 76, 77, 78, 79, 80, 81, 82, 83, 85, 86, 87, 88, 89, 90, 125, 126
BlockMapList38: .db 17 \ .db 74, 75, 76, 77, 78, 79, 85, 86, 87, 88, 89, 90, 91, 92, 124, 125, 126
BlockMapList39: .db 13 \ .db 74, 75, 76, 77, 89, 90, 91, 92, 93, 94, 124, 125, 126
BlockMapList40: .db 9 \ .db 74, 75, 91, 92, 93, 94, 123, 124, 125
BlockMapList41: .db 5 \ .db 92, 93, 94, 123, 124
BlockMapList42: .db 5 \ .db 118, 119, 129, 130, 131
BlockMapList43: .db 9 \ .db 118, 119, 120, 129, 130, 131, 149, 150, 151
BlockMapList44: .db 6 \ .db 120, 147, 148, 149, 150, 151
BlockMapList45: .db 8 \ .db 142, 143, 144, 145, 146, 147, 148, 149
BlockMapList46: .db 11 \ .db 137, 138, 139, 140, 142, 143, 144, 145, 146, 147, 163
BlockMapList47: .db 7 \ .db 137, 138, 139, 140, 142, 143, 163
BlockMapList48: .db 8 \ .db 33, 60, 78, 79, 80, 81, 87, 88
BlockMapList49: .db 12 \ .db 33, 60, 76, 77, 78, 79, 80, 81, 87, 88, 89, 90
BlockMapList50: .db 16 \ .db 33, 34, 73, 74, 75, 76, 77, 78, 79, 87, 88, 89, 90, 91, 92, 107
BlockMapList51: .db 16 \ .db 33, 34, 73, 74, 75, 76, 77, 89, 90, 91, 92, 93, 94, 95, 107, 108
BlockMapList52: .db 13 \ .db 35, 36, 73, 74, 75, 91, 92, 93, 94, 95, 108, 122, 123
BlockMapList53: .db 9 \ .db 35, 36, 92, 93, 94, 95, 108, 122, 123
BlockMapList54: .db 7 \ .db 36, 37, 118, 119, 121, 122, 129
BlockMapList55: .db 9 \ .db 36, 37, 118, 119, 120, 121, 122, 129, 151
BlockMapList56: .db 5 \ .db 120, 121, 151, 164, 176
BlockMapList57: .db 8 \ .db 142, 143, 144, 145, 164, 165, 176, 182
BlockMapList58: .db 12 \ .db 139, 140, 142, 143, 144, 145, 163, 164, 165, 166, 182, 186
BlockMapList59: .db 10 \ .db 139, 140, 142, 143, 163, 165, 166, 167, 182, 186
BlockMapList60: .db 5 \ .db 166, 167, 168, 232, 233
BlockMapList61: .db 5 \ .db 167, 168, 232, 233, 234
BlockMapList62: .db 2 \ .db 233, 234
BlockMapList63: .db 4 \ .db 233, 234, 235, 236
BlockMapList64: .db 5 \ .db 234, 235, 236, 237, 238
BlockMapList65: .db 3 \ .db 236, 237, 238
BlockMapList66: .db 6 \ .db 33, 59, 60, 78, 79, 80
BlockMapList67: .db 8 \ .db 33, 59, 60, 76, 77, 78, 79, 80
BlockMapList68: .db 10 \ .db 33, 34, 73, 74, 75, 76, 77, 78, 79, 107
BlockMapList69: .db 12 \ .db 33, 34, 73, 74, 75, 76, 77, 93, 94, 95, 107, 108
BlockMapList70: .db 12 \ .db 35, 36, 73, 74, 75, 93, 94, 95, 107, 108, 122, 123
BlockMapList71: .db 8 \ .db 35, 36, 93, 94, 95, 108, 122, 123
BlockMapList72: .db 5 \ .db 36, 37, 38, 121, 122
BlockMapList73: .db 6 \ .db 36, 37, 38, 120, 121, 122
BlockMapList74: .db 5 \ .db 120, 121, 164, 175, 176
BlockMapList75: .db 7 \ .db 142, 164, 165, 175, 176, 182, 183
BlockMapList76: .db 10 \ .db 142, 163, 164, 165, 166, 182, 183, 184, 185, 186
BlockMapList77: .db 10 \ .db 142, 163, 165, 166, 167, 182, 183, 184, 185, 186
BlockMapList78: .db 6 \ .db 166, 167, 168, 169, 232, 233
BlockMapList79: .db 8 \ .db 167, 168, 169, 232, 233, 234, 247, 253
BlockMapList80: .db 4 \ .db 233, 234, 247, 253
BlockMapList81: .db 9 \ .db 233, 234, 235, 236, 247, 248, 249, 253, 255
BlockMapList82: .db 11 \ .db 234, 235, 236, 237, 238, 239, 240, 247, 248, 249, 255
BlockMapList83: .db 5 \ .db 236, 237, 238, 239, 240
BlockMapList84: .db 4 \ .db 57, 58, 59, 60
BlockMapList85: .db 4 \ .db 34, 57, 73, 107
BlockMapList86: .db 5 \ .db 34, 73, 95, 107, 108
BlockMapList87: .db 5 \ .db 35, 73, 95, 107, 108
BlockMapList88: .db 4 \ .db 35, 40, 95, 108
BlockMapList89: .db 4 \ .db 37, 38, 39, 40
BlockMapList90: .db 3 \ .db 174, 175, 176
BlockMapList91: .db 5 \ .db 174, 175, 176, 182, 183
BlockMapList92: .db 5 \ .db 182, 183, 184, 185, 186
BlockMapList93: .db 5 \ .db 168, 169, 170, 231, 232
BlockMapList94: .db 8 \ .db 168, 169, 170, 231, 232, 247, 252, 253
BlockMapList95: .db 3 \ .db 247, 252, 253
BlockMapList96: .db 8 \ .db 247, 248, 249, 250, 251, 252, 253, 255
BlockMapList97: .db 11 \ .db 238, 239, 240, 241, 247, 248, 249, 250, 251, 252, 255
BlockMapList98: .db 6 \ .db 238, 239, 240, 241, 249, 250
BlockMapList99: .db 4 \ .db 56, 57, 58, 59
BlockMapList100: .db 13 \ .db 1, 2, 3, 51, 52, 53, 54, 55, 56, 57, 58, 59, 72
BlockMapList101: .db 13 \ .db 1, 2, 3, 48, 49, 51, 52, 53, 54, 55, 56, 57, 72
BlockMapList102: .db 8 \ .db 3, 47, 48, 49, 51, 52, 53, 72
BlockMapList103: .db 8 \ .db 9, 10, 44, 45, 46, 47, 48, 71
BlockMapList104: .db 12 \ .db 9, 10, 11, 40, 41, 42, 43, 44, 45, 46, 47, 71
BlockMapList105: .db 10 \ .db 11, 38, 39, 40, 41, 42, 43, 71, 218, 219
BlockMapList106: .db 7 \ .db 38, 39, 40, 194, 218, 219, 220
BlockMapList107: .db 6 \ .db 174, 175, 193, 194, 219, 220
BlockMapList108: .db 5 \ .db 169, 170, 171, 231, 232
BlockMapList109: .db 7 \ .db 169, 170, 171, 231, 232, 252, 253
BlockMapList110: .db 2 \ .db 252, 253
BlockMapList111: .db 7 \ .db 248, 249, 250, 251, 252, 253, 255
BlockMapList112: .db 11 \ .db 239, 240, 241, 242, 243, 248, 249, 250, 251, 252, 255
BlockMapList113: .db 7 \ .db 239, 240, 241, 242, 243, 249, 250
BlockMapList114: .db 5 \ .db 0, 19, 24, 56, 57
BlockMapList115: .db 15 \ .db 0, 1, 2, 3, 4, 19, 24, 51, 52, 53, 54, 55, 56, 57, 72
BlockMapList116: .db 18 \ .db 0, 1, 2, 3, 4, 5, 6, 24, 48, 49, 51, 52, 53, 54, 55, 56, 57, 72
BlockMapList117: .db 16 \ .db 2, 3, 4, 5, 6, 7, 8, 24, 25, 47, 48, 49, 51, 52, 53, 72
BlockMapList118: .db 11 \ .db 7, 8, 9, 10, 25, 44, 45, 46, 47, 48, 71
BlockMapList119: .db 16 \ .db 8, 9, 10, 11, 12, 13, 25, 40, 41, 42, 43, 44, 45, 46, 47, 71
BlockMapList120: .db 12 \ .db 10, 11, 12, 13, 25, 40, 41, 42, 43, 71, 218, 219
BlockMapList121: .db 7 \ .db 40, 194, 195, 197, 218, 219, 220
BlockMapList122: .db 9 \ .db 174, 191, 192, 193, 194, 195, 197, 219, 220
BlockMapList123: .db 10 \ .db 174, 191, 192, 193, 194, 195, 197, 199, 219, 220
BlockMapList124: .db 4 \ .db 172, 191, 199, 200
BlockMapList125: .db 7 \ .db 170, 171, 172, 203, 204, 230, 231
BlockMapList126: .db 10 \ .db 170, 171, 172, 203, 204, 205, 226, 228, 230, 231
BlockMapList127: .db 12 \ .db 203, 204, 205, 207, 208, 209, 226, 227, 228, 229, 230, 246
BlockMapList128: .db 8 \ .db 207, 208, 209, 227, 229, 244, 245, 246
BlockMapList129: .db 7 \ .db 209, 241, 242, 243, 244, 245, 246
BlockMapList130: .db 4 \ .db 241, 242, 243, 244
BlockMapList131: .db 3 \ .db 0, 19, 24
BlockMapList132: .db 9 \ .db 0, 1, 2, 3, 4, 19, 24, 53, 54
BlockMapList133: .db 12 \ .db 0, 1, 2, 3, 4, 5, 6, 24, 48, 49, 53, 54
BlockMapList134: .db 13 \ .db 2, 3, 4, 5, 6, 7, 8, 24, 25, 47, 48, 49, 53
BlockMapList135: .db 9 \ .db 6, 7, 8, 9, 10, 25, 44, 47, 48
BlockMapList136: .db 11 \ .db 7, 8, 9, 10, 11, 12, 13, 25, 43, 44, 47
BlockMapList137: .db 7 \ .db 10, 11, 12, 13, 25, 43, 218
BlockMapList138: .db 4 \ .db 195, 197, 198, 218
BlockMapList139: .db 5 \ .db 191, 192, 195, 197, 198
BlockMapList140: .db 6 \ .db 191, 192, 195, 197, 198, 199
BlockMapList141: .db 6 \ .db 172, 191, 198, 199, 200, 202
BlockMapList142: .db 7 \ .db 171, 172, 202, 203, 204, 230, 231
BlockMapList143: .db 10 \ .db 171, 172, 202, 203, 204, 205, 226, 228, 230, 231
BlockMapList144: .db 9 \ .db 207, 208, 209, 210, 227, 229, 244, 245, 246
BlockMapList145: .db 6 \ .db 209, 210, 243, 244, 245, 246
BlockMapList146: .db 2 \ .db 243, 244
BlockMapList147: .db 4 \ .db 0, 4, 19, 24
BlockMapList148: .db 6 \ .db 0, 4, 5, 6, 24, 30
BlockMapList149: .db 9 \ .db 4, 5, 6, 7, 8, 24, 25, 30, 31
BlockMapList150: .db 5 \ .db 6, 7, 8, 25, 31
BlockMapList151: .db 5 \ .db 7, 8, 12, 13, 25
BlockMapList152: .db 5 \ .db 12, 13, 25, 217, 218
BlockMapList153: .db 5 \ .db 197, 198, 217, 218, 225
BlockMapList154: .db 6 \ .db 191, 192, 197, 198, 217, 225
BlockMapList155: .db 7 \ .db 191, 192, 197, 198, 199, 217, 225
BlockMapList156: .db 8 \ .db 172, 191, 198, 199, 200, 202, 216, 225
BlockMapList157: .db 7 \ .db 171, 172, 202, 203, 204, 215, 216
BlockMapList158: .db 8 \ .db 171, 172, 202, 203, 204, 205, 215, 216
BlockMapList159: .db 8 \ .db 203, 204, 205, 207, 208, 209, 211, 215
BlockMapList160: .db 5 \ .db 207, 208, 209, 210, 211
BlockMapList161: .db 3 \ .db 209, 210, 211
BlockMapList162: .db 3 \ .db 18, 19, 24
BlockMapList163: .db 4 \ .db 17, 18, 19, 24
BlockMapList164: .db 5 \ .db 16, 17, 18, 24, 30
BlockMapList165: .db 7 \ .db 15, 16, 17, 24, 25, 30, 31
BlockMapList166: .db 4 \ .db 15, 16, 25, 31
BlockMapList167: .db 4 \ .db 13, 14, 15, 25
BlockMapList168: .db 6 \ .db 13, 14, 15, 25, 217, 218
BlockMapList169: .db 3 \ .db 217, 218, 225
BlockMapList170: .db 2 \ .db 217, 225
BlockMapList171: .db 2 \ .db 216, 225
BlockMapList172: .db 3 \ .db 214, 215, 216
BlockMapList173: .db 4 \ .db 213, 214, 215, 216
BlockMapList174: .db 5 \ .db 211, 212, 213, 214, 215
BlockMapList175: .db 4 \ .db 210, 211, 212, 213
BlockMapList176: .db 2 \ .db 210, 211
BlockMapList177: .db 2 \ .db 18, 19
BlockMapList178: .db 3 \ .db 17, 18, 19
BlockMapList179: .db 4 \ .db 16, 17, 18, 30
BlockMapList180: .db 5 \ .db 15, 16, 17, 30, 31
BlockMapList181: .db 3 \ .db 15, 16, 31
BlockMapList182: .db 3 \ .db 13, 14, 15
BlockMapList183: .db 2 \ .db 214, 215
BlockMapList184: .db 3 \ .db 213, 214, 215
BlockMapList185: .db 3 \ .db 211, 212, 213
BlockMapList186: .db 1 \ .db 211
