	.dw Vertices, Vertices.Count
	.dw Walls, Walls.Count
	.dw SubSectors
	.dw Sectors
	.dw Tree
	.dw BlockMap
	.dw 6912, -8448, -352 \ .db 192, 0

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
	.dw 3328, -10496
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
Vertices.Count = ($ - Vertices) / 4

Walls:
	Wall0: .db 7 \ .db 32 \ .db 0, 1 \ .dw Sector0, Sector0 \ .dw 8
	Wall1: .db 7 \ .db 64 \ .db 1, 2 \ .dw Sector2, Sector2 \ .dw 4
	Wall2: .db 3 \ .db 0 \ .db 2, 3 \ .dw Sector2, Sector8 \ .dw 9
	Wall3: .db 7 \ .db 192 \ .db 3, 4 \ .dw Sector2, Sector2 \ .dw 4
	Wall4: .db 7 \ .db 224 \ .db 4, 5 \ .dw Sector0, Sector0 \ .dw 8
	Wall5: .db 5 \ .db 0 \ .db 5, 6 \ .dw Sector0, Sector0 \ .dw 1
	Wall6: .db 4 \ .db 0 \ .db 6, 7 \ .dw Sector0, Sector0 \ .dw 16
	Wall7: .db 6 \ .db 0 \ .db 7, 8 \ .dw Sector0, Sector0 \ .dw 1
	Wall8: .db 7 \ .db 32 \ .db 8, 9 \ .dw Sector0, Sector0 \ .dw 8
	Wall9: .db 7 \ .db 64 \ .db 9, 10 \ .dw Sector2, Sector2 \ .dw 4
	Wall10: .db 3 \ .db 0 \ .db 10, 11 \ .dw Sector2, Sector9 \ .dw 9
	Wall11: .db 7 \ .db 192 \ .db 11, 12 \ .dw Sector2, Sector2 \ .dw 4
	Wall12: .db 7 \ .db 224 \ .db 12, 13 \ .dw Sector0, Sector0 \ .dw 8
	Wall13: .db 7 \ .db 192 \ .db 13, 14 \ .dw Sector1, Sector1 \ .dw 100
	Wall14: .db 4 \ .db 160 \ .db 14, 15 \ .dw Sector1, Sector1 \ .dw 8
	Wall15: .db 5 \ .db 128 \ .db 15, 16 \ .dw Sector1, Sector1 \ .dw 36
	Wall16: .db 4 \ .db 128 \ .db 16, 17 \ .dw Sector1, Sector1 \ .dw 16
	Wall17: .db 6 \ .db 128 \ .db 17, 18 \ .dw Sector1, Sector1 \ .dw 36
	Wall18: .db 4 \ .db 96 \ .db 18, 19 \ .dw Sector1, Sector1 \ .dw 8
	Wall19: .db 7 \ .db 64 \ .db 19, 0 \ .dw Sector1, Sector1 \ .dw 100
	Wall20: .db 3 \ .db 0 \ .db 1, 4 \ .dw Sector0, Sector2 \ .dw 9
	Wall21: .db 3 \ .db 128 \ .db 4, 1 \ .dw Sector2, Sector0 \ .dw 9
	Wall22: .db 3 \ .db 0 \ .db 9, 12 \ .dw Sector0, Sector2 \ .dw 9
	Wall23: .db 3 \ .db 128 \ .db 12, 9 \ .dw Sector2, Sector0 \ .dw 9
	Wall24: .db 3 \ .db 230 \ .db 0, 20 \ .dw Sector1, Sector0 \ .dw 100
	Wall25: .db 3 \ .db 26 \ .db 21, 13 \ .dw Sector1, Sector0 \ .dw 100
	Wall26: .db 3 \ .db 128 \ .db 21, 20 \ .dw Sector0, Sector3 \ .dw 16
	Wall27: .db 3 \ .db 128 \ .db 23, 22 \ .dw Sector3, Sector4 \ .dw 16
	Wall28: .db 3 \ .db 128 \ .db 25, 24 \ .dw Sector4, Sector1 \ .dw 16
	Wall29: .db 1 \ .db 64 \ .db 24, 22 \ .dw Sector4, Sector1 \ .dw 4
	Wall30: .db 2 \ .db 64 \ .db 22, 20 \ .dw Sector3, Sector1 \ .dw 4
	Wall31: .db 1 \ .db 192 \ .db 21, 23 \ .dw Sector3, Sector1 \ .dw 4
	Wall32: .db 2 \ .db 192 \ .db 23, 25 \ .dw Sector4, Sector1 \ .dw 4
	Wall33: .db 7 \ .db 0 \ .db 26, 27 \ .dw Sector5, Sector5 \ .dw 64
	Wall34: .db 7 \ .db 192 \ .db 27, 28 \ .dw Sector5, Sector5 \ .dw 4
	Wall35: .db 7 \ .db 64 \ .db 29, 30 \ .dw Sector5, Sector5 \ .dw 4
	Wall36: .db 7 \ .db 0 \ .db 30, 31 \ .dw Sector5, Sector5 \ .dw 64
	Wall37: .db 5 \ .db 192 \ .db 31, 32 \ .dw Sector5, Sector5 \ .dw 25
	Wall38: .db 4 \ .db 192 \ .db 32, 33 \ .dw Sector5, Sector5 \ .dw 9
	Wall39: .db 6 \ .db 192 \ .db 33, 34 \ .dw Sector5, Sector5 \ .dw 1
	Wall40: .db 7 \ .db 160 \ .db 34, 35 \ .dw Sector5, Sector5 \ .dw 18
	Wall41: .db 7 \ .db 128 \ .db 35, 36 \ .dw Sector5, Sector5 \ .dw 1
	Wall42: .db 7 \ .db 192 \ .db 36, 37 \ .dw Sector5, Sector5 \ .dw 1
	Wall43: .db 7 \ .db 192 \ .db 37, 11 \ .dw Sector9, Sector9 \ .dw 1
	Wall44: .db 7 \ .db 64 \ .db 10, 38 \ .dw Sector9, Sector9 \ .dw 1
	Wall45: .db 7 \ .db 64 \ .db 38, 39 \ .dw Sector5, Sector5 \ .dw 1
	Wall46: .db 7 \ .db 128 \ .db 39, 40 \ .dw Sector5, Sector5 \ .dw 1
	Wall47: .db 7 \ .db 160 \ .db 40, 41 \ .dw Sector7, Sector7 \ .dw 8
	Wall48: .db 7 \ .db 128 \ .db 41, 42 \ .dw Sector7, Sector7 \ .dw 16
	Wall49: .db 7 \ .db 96 \ .db 42, 43 \ .dw Sector7, Sector7 \ .dw 8
	Wall50: .db 3 \ .db 128 \ .db 40, 43 \ .dw Sector5, Sector7 \ .dw 64
	Wall51: .db 7 \ .db 128 \ .db 43, 44 \ .dw Sector5, Sector5 \ .dw 1
	Wall52: .db 7 \ .db 192 \ .db 44, 45 \ .dw Sector5, Sector5 \ .dw 1
	Wall53: .db 7 \ .db 192 \ .db 45, 3 \ .dw Sector8, Sector8 \ .dw 1
	Wall54: .db 7 \ .db 64 \ .db 2, 46 \ .dw Sector8, Sector8 \ .dw 1
	Wall55: .db 7 \ .db 64 \ .db 46, 47 \ .dw Sector5, Sector5 \ .dw 1
	Wall56: .db 7 \ .db 128 \ .db 47, 48 \ .dw Sector5, Sector5 \ .dw 1
	Wall57: .db 7 \ .db 96 \ .db 48, 49 \ .dw Sector5, Sector5 \ .dw 18
	Wall58: .db 5 \ .db 64 \ .db 49, 50 \ .dw Sector5, Sector5 \ .dw 1
	Wall59: .db 4 \ .db 64 \ .db 50, 51 \ .dw Sector5, Sector5 \ .dw 9
	Wall60: .db 6 \ .db 64 \ .db 51, 26 \ .dw Sector5, Sector5 \ .dw 25
	Wall61: .db 1 \ .db 0 \ .db 52, 53 \ .dw Sector6, Sector5 \ .dw 36
	Wall62: .db 0 \ .db 0 \ .db 53, 54 \ .dw Sector6, Sector5 \ .dw 64
	Wall63: .db 2 \ .db 0 \ .db 54, 55 \ .dw Sector6, Sector5 \ .dw 36
	Wall64: .db 3 \ .db 192 \ .db 55, 56 \ .dw Sector6, Sector5 \ .dw 9
	Wall65: .db 3 \ .db 160 \ .db 56, 57 \ .dw Sector6, Sector5 \ .dw 8
	Wall66: .db 1 \ .db 128 \ .db 57, 58 \ .dw Sector6, Sector5 \ .dw 16
	Wall67: .db 0 \ .db 128 \ .db 58, 59 \ .dw Sector6, Sector5 \ .dw 64
	Wall68: .db 2 \ .db 128 \ .db 59, 60 \ .dw Sector6, Sector5 \ .dw 16
	Wall69: .db 3 \ .db 96 \ .db 60, 61 \ .dw Sector6, Sector5 \ .dw 8
	Wall70: .db 3 \ .db 64 \ .db 61, 52 \ .dw Sector6, Sector5 \ .dw 9
	Wall71: .db 3 \ .db 0 \ .db 38, 37 \ .dw Sector9, Sector5 \ .dw 9
	Wall72: .db 3 \ .db 0 \ .db 46, 45 \ .dw Sector8, Sector5 \ .dw 9
	Wall73: .db 5 \ .db 64 \ .db 62, 63 \ .dw Sector10, Sector10 \ .dw 4
	Wall74: .db 6 \ .db 64 \ .db 63, 64 \ .dw Sector11, Sector11 \ .dw 4
	Wall75: .db 5 \ .db 128 \ .db 64, 65 \ .dw Sector13, Sector13 \ .dw 4
	Wall76: .db 4 \ .db 128 \ .db 65, 66 \ .dw Sector14, Sector14 \ .dw 4
	Wall77: .db 4 \ .db 128 \ .db 66, 67 \ .dw Sector15, Sector15 \ .dw 4
	Wall78: .db 4 \ .db 128 \ .db 67, 68 \ .dw Sector16, Sector16 \ .dw 4
	Wall79: .db 6 \ .db 128 \ .db 68, 69 \ .dw Sector17, Sector17 \ .dw 16
	Wall80: .db 5 \ .db 64 \ .db 69, 70 \ .dw Sector17, Sector17 \ .dw 16
	Wall81: .db 4 \ .db 64 \ .db 70, 71 \ .dw Sector18, Sector18 \ .dw 4
	Wall82: .db 4 \ .db 64 \ .db 71, 72 \ .dw Sector19, Sector19 \ .dw 4
	Wall83: .db 6 \ .db 64 \ .db 72, 73 \ .dw Sector20, Sector20 \ .dw 16
	Wall84: .db 3 \ .db 0 \ .db 73, 74 \ .dw Sector20, Sector21 \ .dw 16
	Wall85: .db 5 \ .db 192 \ .db 74, 75 \ .dw Sector20, Sector20 \ .dw 16
	Wall86: .db 4 \ .db 192 \ .db 75, 76 \ .dw Sector19, Sector19 \ .dw 4
	Wall87: .db 6 \ .db 192 \ .db 76, 77 \ .dw Sector18, Sector18 \ .dw 4
	Wall88: .db 5 \ .db 0 \ .db 77, 78 \ .dw Sector16, Sector16 \ .dw 4
	Wall89: .db 4 \ .db 0 \ .db 78, 79 \ .dw Sector15, Sector15 \ .dw 4
	Wall90: .db 4 \ .db 0 \ .db 79, 80 \ .dw Sector14, Sector14 \ .dw 4
	Wall91: .db 4 \ .db 0 \ .db 80, 81 \ .dw Sector13, Sector13 \ .dw 4
	Wall92: .db 6 \ .db 0 \ .db 81, 82 \ .dw Sector12, Sector12 \ .dw 16
	Wall93: .db 5 \ .db 192 \ .db 82, 83 \ .dw Sector12, Sector12 \ .dw 16
	Wall94: .db 4 \ .db 192 \ .db 83, 84 \ .dw Sector11, Sector11 \ .dw 4
	Wall95: .db 6 \ .db 192 \ .db 84, 85 \ .dw Sector10, Sector10 \ .dw 4
	Wall96: .db 0 \ .db 0 \ .db 62, 85 \ .dw Sector5, Sector10 \ .dw 16
	Wall97: .db 3 \ .db 0 \ .db 63, 84 \ .dw Sector10, Sector11 \ .dw 16
	Wall98: .db 3 \ .db 0 \ .db 64, 83 \ .dw Sector11, Sector12 \ .dw 16
	Wall99: .db 3 \ .db 64 \ .db 64, 81 \ .dw Sector12, Sector13 \ .dw 16
	Wall100: .db 3 \ .db 64 \ .db 65, 80 \ .dw Sector13, Sector14 \ .dw 16
	Wall101: .db 3 \ .db 64 \ .db 66, 79 \ .dw Sector14, Sector15 \ .dw 16
	Wall102: .db 3 \ .db 64 \ .db 67, 78 \ .dw Sector15, Sector16 \ .dw 16
	Wall103: .db 3 \ .db 64 \ .db 68, 77 \ .dw Sector16, Sector17 \ .dw 16
	Wall104: .db 3 \ .db 0 \ .db 70, 77 \ .dw Sector17, Sector18 \ .dw 16
	Wall105: .db 3 \ .db 0 \ .db 71, 76 \ .dw Sector18, Sector19 \ .dw 16
	Wall106: .db 3 \ .db 0 \ .db 72, 75 \ .dw Sector19, Sector20 \ .dw 16
	Wall107: .db 7 \ .db 0 \ .db 28, 62 \ .dw Sector5, Sector5 \ .dw 9
	Wall108: .db 7 \ .db 0 \ .db 85, 29 \ .dw Sector5, Sector5 \ .dw 1
	Wall109: .db 7 \ .db 64 \ .db 73, 86 \ .dw Sector21, Sector21 \ .dw 16
	Wall110: .db 5 \ .db 0 \ .db 86, 87 \ .dw Sector21, Sector21 \ .dw 16
	Wall111: .db 6 \ .db 0 \ .db 87, 88 \ .dw Sector21, Sector21 \ .dw 4
	Wall112: .db 7 \ .db 221 \ .db 88, 89 \ .dw Sector21, Sector21 \ .dw 85
	Wall113: .db 5 \ .db 0 \ .db 89, 90 \ .dw Sector21, Sector21 \ .dw 16
	Wall114: .db 6 \ .db 0 \ .db 90, 91 \ .dw Sector21, Sector21 \ .dw 16
	Wall115: .db 7 \ .db 32 \ .db 91, 92 \ .dw Sector21, Sector21 \ .dw 18
	Wall116: .db 7 \ .db 0 \ .db 92, 93 \ .dw Sector21, Sector21 \ .dw 9
	Wall117: .db 5 \ .db 192 \ .db 93, 94 \ .dw Sector21, Sector21 \ .dw 9
	Wall118: .db 6 \ .db 192 \ .db 94, 95 \ .dw Sector21, Sector21 \ .dw 36
	Wall119: .db 7 \ .db 0 \ .db 95, 96 \ .dw Sector22, Sector22 \ .dw 25
	Wall120: .db 7 \ .db 192 \ .db 96, 97 \ .dw Sector22, Sector22 \ .dw 16
	Wall121: .db 5 \ .db 128 \ .db 97, 98 \ .dw Sector22, Sector22 \ .dw 25
	Wall122: .db 6 \ .db 128 \ .db 98, 99 \ .dw Sector21, Sector21 \ .dw 81
	Wall123: .db 7 \ .db 64 \ .db 99, 100 \ .dw Sector21, Sector21 \ .dw 36
	Wall124: .db 5 \ .db 128 \ .db 100, 101 \ .dw Sector21, Sector21 \ .dw 25
	Wall125: .db 6 \ .db 128 \ .db 101, 102 \ .dw Sector21, Sector21 \ .dw 16
	Wall126: .db 7 \ .db 75 \ .db 102, 103 \ .dw Sector21, Sector21 \ .dw 53
	Wall127: .db 7 \ .db 128 \ .db 103, 74 \ .dw Sector21, Sector21 \ .dw 4
	Wall128: .db 2 \ .db 192 \ .db 95, 98 \ .dw Sector21, Sector22 \ .dw 16
Walls.Count = ($ - Walls) / 10

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
Sector10: .dw -128, 24
Sector11: .dw -160, 24
Sector12: .dw -192, 24
Sector13: .dw -224, 24
Sector14: .dw -256, 24
Sector15: .dw -288, 24
Sector16: .dw -320, 24
Sector17: .dw -352, 24
Sector18: .dw -384, 24
Sector19: .dw -416, 24
Sector20: .dw -448, 24
Sector21: .dw -448, -192
Sector22: .dw -448, -256

SubSectors:
SubSector0: .dw Sector0
	.db 0
	.db 4 \ .dw Wall5, Wall0, Wall4, Wall20

SubSector1: .dw Sector2
	.db 0
	.db 4 \ .dw Wall1, Wall2, Wall3, Wall21

SubSector2: .dw Sector0
	.db 0
	.db 4 \ .dw Wall7, Wall8, Wall12, Wall22

SubSector3: .dw Sector2
	.db 0
	.db 4 \ .dw Wall9, Wall10, Wall11, Wall23

SubSector4: .dw Sector0
	.db 0
	.db 1 \ .dw Wall24

SubSector5: .dw Sector1
	.db 1 \ .dw Thing0
	.db 6 \ .dw Wall29, Wall30, Wall19, Wall17, Wall24, Wall18

SubSector6: .dw Sector0
	.db 0
	.db 1 \ .dw Wall25

SubSector7: .dw Sector1
	.db 0
	.db 6 \ .dw Wall13, Wall31, Wall32, Wall15, Wall25, Wall14

SubSector8: .dw Sector0
	.db 0
	.db 2 \ .dw Wall26, Wall6

SubSector9: .dw Sector3
	.db 0
	.db 4 \ .dw Wall26, Wall27, Wall30, Wall31

SubSector10: .dw Sector4
	.db 1 \ .dw Thing2
	.db 4 \ .dw Wall27, Wall28, Wall29, Wall32

SubSector11: .dw Sector1
	.db 0
	.db 2 \ .dw Wall28, Wall16

SubSector12: .dw Sector5
	.db 0
	.db 4 \ .dw Wall62, Wall96, Wall107, Wall108

SubSector13: .dw Sector6
	.db 0
	.db 2 \ .dw Wall62, Wall67

SubSector14: .dw Sector5
	.db 0
	.db 2 \ .dw Wall67, Wall50

SubSector15: .dw Sector7
	.db 1 \ .dw Thing1
	.db 4 \ .dw Wall50, Wall47, Wall48, Wall49

SubSector16: .dw Sector5
	.db 0
	.db 4 \ .dw Wall61, Wall33, Wall34, Wall60

SubSector17: .dw Sector5
	.db 0
	.db 2 \ .dw Wall59, Wall70

SubSector18: .dw Sector6
	.db 1 \ .dw Thing3
	.db 2 \ .dw Wall61, Wall70

SubSector19: .dw Sector6
	.db 0
	.db 1 \ .dw Wall69

SubSector20: .dw Sector5
	.db 0
	.db 4 \ .dw Wall56, Wall57, Wall58, Wall69

SubSector21: .dw Sector6
	.db 0
	.db 1 \ .dw Wall68

SubSector22: .dw Sector5
	.db 0
	.db 2 \ .dw Wall68, Wall51

SubSector23: .dw Sector6
	.db 1 \ .dw Thing4
	.db 2 \ .dw Wall63, Wall64

SubSector24: .dw Sector5
	.db 0
	.db 2 \ .dw Wall64, Wall38

SubSector25: .dw Sector6
	.db 0
	.db 1 \ .dw Wall66

SubSector26: .dw Sector6
	.db 0
	.db 1 \ .dw Wall65

SubSector27: .dw Sector5
	.db 0
	.db 4 \ .dw Wall65, Wall39, Wall40, Wall41

SubSector28: .dw Sector5
	.db 0
	.db 2 \ .dw Wall66, Wall46

SubSector29: .dw Sector5
	.db 0
	.db 4 \ .dw Wall63, Wall35, Wall36, Wall37

SubSector30: .dw Sector5
	.db 0
	.db 3 \ .dw Wall71, Wall42, Wall45

SubSector31: .dw Sector5
	.db 0
	.db 3 \ .dw Wall72, Wall52, Wall55

SubSector32: .dw Sector8
	.db 0
	.db 4 \ .dw Wall2, Wall72, Wall53, Wall54

SubSector33: .dw Sector9
	.db 0
	.db 4 \ .dw Wall10, Wall43, Wall44, Wall71

SubSector34: .dw Sector10
	.db 0
	.db 4 \ .dw Wall96, Wall97, Wall73, Wall95

SubSector35: .dw Sector11
	.db 0
	.db 4 \ .dw Wall97, Wall98, Wall74, Wall94

SubSector36: .dw Sector12
	.db 0
	.db 4 \ .dw Wall98, Wall99, Wall92, Wall93

SubSector37: .dw Sector13
	.db 0
	.db 4 \ .dw Wall99, Wall100, Wall75, Wall91

SubSector38: .dw Sector14
	.db 0
	.db 4 \ .dw Wall100, Wall101, Wall76, Wall90

SubSector39: .dw Sector15
	.db 0
	.db 4 \ .dw Wall101, Wall102, Wall77, Wall89

SubSector40: .dw Sector16
	.db 0
	.db 4 \ .dw Wall102, Wall103, Wall78, Wall88

SubSector41: .dw Sector17
	.db 1 \ .dw Thing6
	.db 4 \ .dw Wall103, Wall104, Wall79, Wall80

SubSector42: .dw Sector18
	.db 0
	.db 4 \ .dw Wall104, Wall105, Wall81, Wall87

SubSector43: .dw Sector19
	.db 0
	.db 4 \ .dw Wall105, Wall106, Wall82, Wall86

SubSector44: .dw Sector20
	.db 0
	.db 4 \ .dw Wall106, Wall84, Wall83, Wall85

SubSector45: .dw Sector21
	.db 0
	.db 3 \ .dw Wall84, Wall109, Wall110

SubSector46: .dw Sector21
	.db 0
	.db 2 \ .dw Wall111, Wall127

SubSector47: .dw Sector21
	.db 0
	.db 3 \ .dw Wall112, Wall126, Wall125

SubSector48: .dw Sector21
	.db 0
	.db 5 \ .dw Wall128, Wall114, Wall118, Wall122, Wall123

SubSector49: .dw Sector21
	.db 1 \ .dw Thing5
	.db 3 \ .dw Wall115, Wall116, Wall117

SubSector50: .dw Sector22
	.db 0
	.db 4 \ .dw Wall128, Wall119, Wall120, Wall121

SubSector51: .dw Sector21
	.db 0
	.db 2 \ .dw Wall113, Wall124


Things:
Thing0: .dw 1024, -1280 \ .dw Thing.Stand
Thing1: .dw 2560, -4352 \ .dw Thing.Stand
Thing2: .dw 2816, -768 \ .dw Thing.Keycard
Thing3: .dw 640, -5760 \ .dw Thing.Lamp
Thing4: .dw 4480, -5760 \ .dw Thing.Lamp
Thing5: .dw 5376, -10880 \ .dw Thing.Stand
Thing6: .dw -512, -8448 \ .dw Thing.Lamp

Tree:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F
.dw Tree.B
.dw -7680

Tree.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F
.dw Tree.F.B
.dw -4096

Tree.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F
.dw Tree.F.F.B
.dw -3072

Tree.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.F
.dw Tree.F.F.F.B
.dw 2048

Tree.F.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.F.F.F
.dw Tree.F.F.F.F.B
.dw 3072

Tree.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.F.F.F.F
.dw Tree.F.F.F.F.F.B
.dw 768
.dw -192

Tree.F.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector7

Tree.F.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector6

Tree.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.F.B.F
.dw Tree.F.F.F.F.B.B
.dw -1024

Tree.F.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.F.B.F.F
.dw Tree.F.F.F.F.B.F.B
.dw -512

Tree.F.F.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector11

Tree.F.F.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector10

Tree.F.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.F.F.B.B.F
.dw Tree.F.F.F.F.B.B.B
.dw -1536

Tree.F.F.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector9

Tree.F.F.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector8

Tree.F.F.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.F.F.B.F
.dw Tree.F.F.F.B.B
.dw -3072
.dw 192

Tree.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector5

Tree.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector4

Tree.F.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.F.B.F
.dw Tree.F.F.B.B
.dw 2560

Tree.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.F.F
.dw Tree.F.F.B.F.B
.dw -3584

Tree.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector2

Tree.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector3

Tree.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.F.B.B.F
.dw Tree.F.F.B.B.B
.dw -3584

Tree.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector0

Tree.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector1

Tree.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.F
.dw Tree.F.B.B
.dw 1536

Tree.F.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.F.F
.dw Tree.F.B.F.B
.dw 3584

Tree.F.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F.F
.dw Tree.F.B.F.F.B
.dw -5632

Tree.F.B.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.F.F.F.F
.dw Tree.F.B.F.F.F.B
.dw 4608

Tree.F.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.B.F.F.F.F.F
.dw Tree.F.B.F.F.F.F.B
.dw -512
.dw 256

Tree.F.B.F.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector27

Tree.F.B.F.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector26

Tree.F.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F.F.B.F
.dw Tree.F.B.F.F.F.B.B
.dw -4608

Tree.F.B.F.F.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F.F.B.F.F
.dw Tree.F.B.F.F.F.B.F.B
.dw -4352

Tree.F.B.F.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector33

Tree.F.B.F.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector30

Tree.F.B.F.F.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F.F.B.B.F
.dw Tree.F.B.F.F.F.B.B.B
.dw -5120

Tree.F.B.F.F.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector28

Tree.F.B.F.F.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector25

Tree.F.B.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.F.B.F
.dw Tree.F.B.F.F.B.B
.dw -6400

Tree.F.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.F.F.B.F.F
.dw Tree.F.B.F.F.B.F.B
.dw 5120

Tree.F.B.F.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector24

Tree.F.B.F.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector23

Tree.F.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector29

Tree.F.B.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.B.F
.dw Tree.F.B.F.B.B
.dw -5120

Tree.F.B.F.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.B.F.F
.dw Tree.F.B.F.B.F.B
.dw -4608

Tree.F.B.F.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector15

Tree.F.B.F.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector14

Tree.F.B.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.B.B.F
.dw Tree.F.B.F.B.B.B
.dw -6400

Tree.F.B.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector13

Tree.F.B.F.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.F.B.B.B.F
.dw Tree.F.B.F.B.B.B.B
.dw -7168

Tree.F.B.F.B.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector12

Tree.F.B.F.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector34

Tree.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.B.F
.dw Tree.F.B.B.B
.dw -5632

Tree.F.B.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.B.F.F
.dw Tree.F.B.B.F.B
.dw 512

Tree.F.B.B.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.B.F.F.F
.dw Tree.F.B.B.F.F.B
.dw -4608

Tree.F.B.B.F.F.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.B.F.F.F.F
.dw Tree.F.B.B.F.F.F.B
.dw -4352

Tree.F.B.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector32

Tree.F.B.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector31

Tree.F.B.B.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.B.F.F.B.F
.dw Tree.F.B.B.F.F.B.B
.dw -5120

Tree.F.B.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector22

Tree.F.B.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector21

Tree.F.B.B.F.B:
.db Nostromo.Tree.Node.Type.ShallowSlopePartition
.dw Tree.F.B.B.F.B.F
.dw Tree.F.B.B.F.B.B
.dw 0
.dw -256

Tree.F.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector20

Tree.F.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector19

Tree.F.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.F.B.B.B.F
.dw Tree.F.B.B.B.B
.dw -6400

Tree.F.B.B.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.F.B.B.B.F.F
.dw Tree.F.B.B.B.F.B
.dw 0

Tree.F.B.B.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector18

Tree.F.B.B.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector17

Tree.F.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector16

Tree.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F
.dw Tree.B.B
.dw 2304

Tree.B.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.F
.dw Tree.B.F.B
.dw 3584

Tree.B.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.F.F.F
.dw Tree.B.F.F.B
.dw 5888

Tree.B.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector50

Tree.B.F.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.F.B.F
.dw Tree.B.F.F.B.B
.dw -10496

Tree.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector48

Tree.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector49

Tree.B.F.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.F
.dw Tree.B.F.B.B
.dw -8192

Tree.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector35

Tree.B.F.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.F.B.B.F
.dw Tree.B.F.B.B.B
.dw -9344

Tree.B.F.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector36

Tree.B.F.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector51

Tree.B.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.F
.dw Tree.B.B.B
.dw 256

Tree.B.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.F.F
.dw Tree.B.B.F.B
.dw -9344

Tree.B.B.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.F.F.F
.dw Tree.B.B.F.F.B
.dw 1280

Tree.B.B.F.F.F:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.F.F.F.F
.dw Tree.B.B.F.F.F.B
.dw 1792

Tree.B.B.F.F.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector37

Tree.B.B.F.F.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector38

Tree.B.B.F.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.F.F.B.F
.dw Tree.B.B.F.F.B.B
.dw 768

Tree.B.B.F.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector39

Tree.B.B.F.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector40

Tree.B.B.F.B:
.db Nostromo.Tree.Node.Type.VerticalPartition
.dw Tree.B.B.F.B.F
.dw Tree.B.B.F.B.B
.dw 768

Tree.B.B.F.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector47

Tree.B.B.F.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector46

Tree.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.F
.dw Tree.B.B.B.B
.dw -9728

Tree.B.B.B.F:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.F.F
.dw Tree.B.B.B.F.B
.dw -9216

Tree.B.B.B.F.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector41

Tree.B.B.B.F.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector42

Tree.B.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.B.F
.dw Tree.B.B.B.B.B
.dw -10240

Tree.B.B.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector43

Tree.B.B.B.B.B:
.db Nostromo.Tree.Node.Type.HorizontalPartition
.dw Tree.B.B.B.B.B.F
.dw Tree.B.B.B.B.B.B
.dw -11264

Tree.B.B.B.B.B.F:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector44

Tree.B.B.B.B.B.B:
.db Nostromo.Tree.Node.Type.Leaf
.dw SubSector45

BlockMap:
.dw -1280, -12800 \ .db 9, 13
.dw BlockMapList0, BlockMapList1, BlockMapList2, BlockMapList3, BlockMapList4, BlockMapList5, BlockMapList6, BlockMapList7, BlockMapList8
.dw BlockMapList9, BlockMapList10, BlockMapList11, BlockMapList12, BlockMapList13, BlockMapList14, BlockMapList15, BlockMapList16, BlockMapList8
.dw BlockMapList17, BlockMapList18, BlockMapList19, BlockMapList20, BlockMapList21, BlockMapList22, BlockMapList15, BlockMapList16, BlockMapList8
.dw BlockMapList23, BlockMapList24, BlockMapList25, BlockMapList26, BlockMapList27, BlockMapList28, BlockMapList29, BlockMapList30, BlockMapList31
.dw BlockMapList32, BlockMapList33, BlockMapList34, BlockMapList35, BlockMapList36, BlockMapList37, BlockMapList38, BlockMapList39, BlockMapList40
.dw BlockMapList41, BlockMapList42, BlockMapList43, BlockMapList44, BlockMapList45, BlockMapList46, BlockMapList47, BlockMapList48, BlockMapList49
.dw BlockMapList50, BlockMapList51, BlockMapList52, BlockMapList53, BlockMapList54, BlockMapList55, BlockMapList56, BlockMapList57, BlockMapList8
.dw BlockMapList58, BlockMapList59, BlockMapList60, BlockMapList61, BlockMapList62, BlockMapList63, BlockMapList64, BlockMapList65, BlockMapList8
.dw BlockMapList66, BlockMapList67, BlockMapList68, BlockMapList69, BlockMapList70, BlockMapList71, BlockMapList72, BlockMapList73, BlockMapList8
.dw BlockMapList74, BlockMapList75, BlockMapList76, BlockMapList77, BlockMapList78, BlockMapList79, BlockMapList80, BlockMapList8, BlockMapList8
.dw BlockMapList74, BlockMapList81, BlockMapList82, BlockMapList83, BlockMapList84, BlockMapList85, BlockMapList86, BlockMapList8, BlockMapList8
.dw BlockMapList87, BlockMapList88, BlockMapList89, BlockMapList90, BlockMapList91, BlockMapList92, BlockMapList92, BlockMapList8, BlockMapList8
.dw BlockMapList93, BlockMapList94, BlockMapList95, BlockMapList96, BlockMapList97, BlockMapList98, BlockMapList98, BlockMapList8, BlockMapList8
BlockMapList0: .db 7 \ .db 83, 84, 85, 109, 110, 111, 127
BlockMapList1: .db 9 \ .db 83, 84, 85, 109, 110, 111, 112, 126, 127
BlockMapList2: .db 6 \ .db 85, 110, 111, 112, 126, 127
BlockMapList3: .db 2 \ .db 112, 126
BlockMapList4: .db 2 \ .db 112, 115
BlockMapList5: .db 2 \ .db 115, 116
BlockMapList6: .db 3 \ .db 115, 116, 117
BlockMapList7: .db 1 \ .db 117
BlockMapList8: .db 0
BlockMapList9: .db 10 \ .db 82, 83, 84, 85, 86, 106, 109, 110, 111, 127
BlockMapList10: .db 12 \ .db 82, 83, 84, 85, 86, 106, 109, 110, 111, 112, 126, 127
BlockMapList11: .db 9 \ .db 84, 85, 86, 110, 111, 112, 113, 126, 127
BlockMapList12: .db 4 \ .db 112, 113, 114, 126
BlockMapList13: .db 4 \ .db 112, 113, 114, 115
BlockMapList14: .db 4 \ .db 113, 114, 115, 116
BlockMapList15: .db 5 \ .db 114, 115, 116, 117, 118
BlockMapList16: .db 3 \ .db 116, 117, 118
BlockMapList17: .db 15 \ .db 80, 81, 82, 83, 84, 85, 86, 87, 88, 103, 104, 105, 106, 109, 127
BlockMapList18: .db 22 \ .db 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 101, 102, 103, 104, 105, 106, 109, 112, 125, 126, 127
BlockMapList19: .db 22 \ .db 84, 85, 86, 87, 88, 89, 90, 91, 92, 99, 100, 101, 102, 103, 105, 106, 112, 113, 124, 125, 126, 127
BlockMapList20: .db 13 \ .db 90, 91, 92, 93, 99, 100, 101, 112, 113, 114, 124, 125, 126
BlockMapList21: .db 10 \ .db 92, 93, 99, 112, 113, 114, 115, 123, 124, 125
BlockMapList22: .db 7 \ .db 93, 113, 114, 115, 116, 123, 124
BlockMapList23: .db 14 \ .db 78, 79, 80, 81, 82, 83, 85, 86, 87, 88, 103, 104, 105, 106
BlockMapList24: .db 22 \ .db 76, 77, 78, 79, 80, 81, 82, 83, 85, 86, 87, 88, 89, 90, 101, 102, 103, 104, 105, 106, 125, 126
BlockMapList25: .db 26 \ .db 74, 75, 76, 77, 78, 79, 85, 86, 87, 88, 89, 90, 91, 92, 98, 99, 100, 101, 102, 103, 104, 105, 106, 124, 125, 126
BlockMapList26: .db 17 \ .db 74, 75, 76, 77, 89, 90, 91, 92, 93, 94, 98, 99, 100, 101, 124, 125, 126
BlockMapList27: .db 11 \ .db 74, 75, 91, 92, 93, 94, 98, 99, 123, 124, 125
BlockMapList28: .db 5 \ .db 92, 93, 94, 123, 124
BlockMapList29: .db 3 \ .db 118, 119, 128
BlockMapList30: .db 4 \ .db 118, 119, 120, 128
BlockMapList31: .db 2 \ .db 119, 120
BlockMapList32: .db 10 \ .db 33, 60, 78, 79, 80, 81, 87, 88, 103, 104
BlockMapList33: .db 16 \ .db 33, 60, 76, 77, 78, 79, 80, 81, 87, 88, 89, 90, 101, 102, 103, 104
BlockMapList34: .db 25 \ .db 33, 34, 73, 74, 75, 76, 77, 78, 79, 87, 88, 89, 90, 91, 92, 96, 97, 98, 99, 100, 101, 102, 103, 104, 107
BlockMapList35: .db 22 \ .db 33, 34, 73, 74, 75, 76, 77, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 107, 108
BlockMapList36: .db 17 \ .db 35, 36, 73, 74, 75, 91, 92, 93, 94, 95, 96, 97, 98, 99, 108, 122, 123
BlockMapList37: .db 11 \ .db 35, 36, 92, 93, 94, 95, 97, 98, 108, 122, 123
BlockMapList38: .db 7 \ .db 36, 37, 118, 119, 121, 122, 128
BlockMapList39: .db 8 \ .db 36, 37, 118, 119, 120, 121, 122, 128
BlockMapList40: .db 3 \ .db 119, 120, 121
BlockMapList41: .db 9 \ .db 33, 59, 60, 61, 70, 78, 79, 80, 103
BlockMapList42: .db 13 \ .db 33, 59, 60, 61, 70, 76, 77, 78, 79, 80, 101, 102, 103
BlockMapList43: .db 20 \ .db 33, 34, 61, 62, 73, 74, 75, 76, 77, 78, 79, 96, 97, 98, 99, 100, 101, 102, 103, 107
BlockMapList44: .db 20 \ .db 33, 34, 61, 62, 73, 74, 75, 76, 77, 93, 94, 95, 96, 97, 98, 99, 100, 101, 107, 108
BlockMapList45: .db 18 \ .db 35, 36, 62, 63, 73, 74, 75, 93, 94, 95, 96, 97, 98, 99, 107, 108, 122, 123
BlockMapList46: .db 14 \ .db 35, 36, 62, 63, 64, 93, 94, 95, 96, 97, 98, 108, 122, 123
BlockMapList47: .db 8 \ .db 36, 37, 38, 63, 64, 121, 122, 128
BlockMapList48: .db 7 \ .db 36, 37, 38, 120, 121, 122, 128
BlockMapList49: .db 2 \ .db 120, 121
BlockMapList50: .db 7 \ .db 57, 58, 59, 60, 61, 69, 70
BlockMapList51: .db 8 \ .db 57, 58, 59, 60, 61, 68, 69, 70
BlockMapList52: .db 10 \ .db 34, 57, 61, 62, 67, 68, 69, 73, 96, 107
BlockMapList53: .db 10 \ .db 34, 61, 62, 67, 68, 73, 95, 96, 107, 108
BlockMapList54: .db 10 \ .db 35, 62, 63, 66, 67, 73, 95, 96, 107, 108
BlockMapList55: .db 11 \ .db 35, 40, 62, 63, 64, 65, 66, 67, 95, 96, 108
BlockMapList56: .db 8 \ .db 37, 38, 39, 40, 63, 64, 65, 66
BlockMapList57: .db 4 \ .db 37, 38, 39, 40
BlockMapList58: .db 6 \ .db 56, 57, 58, 59, 69, 70
BlockMapList59: .db 16 \ .db 1, 2, 3, 51, 52, 53, 54, 55, 56, 57, 58, 59, 68, 69, 70, 72
BlockMapList60: .db 17 \ .db 1, 2, 3, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 67, 68, 69, 72
BlockMapList61: .db 11 \ .db 3, 47, 48, 49, 50, 51, 52, 53, 67, 68, 72
BlockMapList62: .db 11 \ .db 9, 10, 44, 45, 46, 47, 48, 50, 66, 67, 71
BlockMapList63: .db 17 \ .db 9, 10, 11, 40, 41, 42, 43, 44, 45, 46, 47, 50, 64, 65, 66, 67, 71
BlockMapList64: .db 11 \ .db 11, 38, 39, 40, 41, 42, 43, 64, 65, 66, 71
BlockMapList65: .db 3 \ .db 38, 39, 40
BlockMapList66: .db 6 \ .db 0, 19, 24, 56, 57, 69
BlockMapList67: .db 19 \ .db 0, 1, 2, 3, 4, 19, 20, 21, 24, 51, 52, 53, 54, 55, 56, 57, 68, 69, 72
BlockMapList68: .db 24 \ .db 0, 1, 2, 3, 4, 5, 6, 20, 21, 24, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 67, 68, 69, 72
BlockMapList69: .db 21 \ .db 2, 3, 4, 5, 6, 7, 8, 20, 21, 24, 25, 47, 48, 49, 50, 51, 52, 53, 67, 68, 72
BlockMapList70: .db 16 \ .db 7, 8, 9, 10, 22, 23, 25, 44, 45, 46, 47, 48, 50, 66, 67, 71
BlockMapList71: .db 22 \ .db 8, 9, 10, 11, 12, 13, 22, 23, 25, 40, 41, 42, 43, 44, 45, 46, 47, 50, 65, 66, 67, 71
BlockMapList72: .db 14 \ .db 10, 11, 12, 13, 22, 23, 25, 40, 41, 42, 43, 65, 66, 71
BlockMapList73: .db 1 \ .db 40
BlockMapList74: .db 3 \ .db 0, 19, 24
BlockMapList75: .db 11 \ .db 0, 1, 2, 3, 4, 19, 20, 21, 24, 53, 54
BlockMapList76: .db 14 \ .db 0, 1, 2, 3, 4, 5, 6, 20, 21, 24, 48, 49, 53, 54
BlockMapList77: .db 15 \ .db 2, 3, 4, 5, 6, 7, 8, 20, 21, 24, 25, 47, 48, 49, 53
BlockMapList78: .db 11 \ .db 6, 7, 8, 9, 10, 22, 23, 25, 44, 47, 48
BlockMapList79: .db 13 \ .db 7, 8, 9, 10, 11, 12, 13, 22, 23, 25, 43, 44, 47
BlockMapList80: .db 8 \ .db 10, 11, 12, 13, 22, 23, 25, 43
BlockMapList81: .db 4 \ .db 0, 4, 19, 24
BlockMapList82: .db 9 \ .db 0, 4, 5, 6, 24, 26, 27, 29, 30
BlockMapList83: .db 13 \ .db 4, 5, 6, 7, 8, 24, 25, 26, 27, 29, 30, 31, 32
BlockMapList84: .db 8 \ .db 6, 7, 8, 25, 26, 27, 31, 32
BlockMapList85: .db 5 \ .db 7, 8, 12, 13, 25
BlockMapList86: .db 3 \ .db 12, 13, 25
BlockMapList87: .db 3 \ .db 18, 19, 24
BlockMapList88: .db 4 \ .db 17, 18, 19, 24
BlockMapList89: .db 9 \ .db 16, 17, 18, 24, 26, 27, 28, 29, 30
BlockMapList90: .db 12 \ .db 15, 16, 17, 24, 25, 26, 27, 28, 29, 30, 31, 32
BlockMapList91: .db 8 \ .db 15, 16, 25, 26, 27, 28, 31, 32
BlockMapList92: .db 4 \ .db 13, 14, 15, 25
BlockMapList93: .db 2 \ .db 18, 19
BlockMapList94: .db 3 \ .db 17, 18, 19
BlockMapList95: .db 7 \ .db 16, 17, 18, 27, 28, 29, 30
BlockMapList96: .db 9 \ .db 15, 16, 17, 27, 28, 29, 30, 31, 32
BlockMapList97: .db 6 \ .db 15, 16, 27, 28, 31, 32
BlockMapList98: .db 3 \ .db 13, 14, 15
