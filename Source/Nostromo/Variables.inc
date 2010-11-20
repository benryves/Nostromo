.function AllocateVariable(size)
	AllocateVariable = VariableRegion
	VariableRegion += size
	VariableSpace -= size
	.if VariableSpace < 0
		.echoln "Insufficient variable space."
	.endif
.endfunction

VariableRegion1 = TopEdgeClip + 96
VariableRegion2 = BottomEdgeClip + 96

VariableRegion = VariableRegion1 \ VariableSpace = 160

CameraVariables = AllocateVariable(0)
Camera.X = AllocateVariable(2)
Camera.Y = AllocateVariable(2)
Camera.Z = AllocateVariable(2)
Camera.Angle = AllocateVariable(1)
Camera.YShear = AllocateVariable(1)
CameraVariables.Size = AllocateVariable(0) - CameraVariables

Render.Camera.Angle = AllocateVariable(1)
Render.Camera.Z = AllocateVariable(2)
Render.Camera.YShear = AllocateVariable(2)

Sector.Front = AllocateVariable(2)
Sector.Back = AllocateVariable(2)

Previous.Camera.X = AllocateVariable(2)
Previous.Camera.Y = AllocateVariable(2)
Previous.Camera.Angle = AllocateVariable(1)

AllocatedTableMemory = AllocateVariable(2)
ClipTableAddress = AllocateVariable(2)

ColumnsToDraw = AllocateVariable(1)

.module Vertices

SinA = AllocateVariable(2)
CosA = AllocateVariable(2)

.endmodule

.module Wall

Trapezium.Start.Column = AllocateVariable(1)
Trapezium.End.Column = AllocateVariable(1)

Trapezium.Start.Ceiling = AllocateVariable(2)
Trapezium.Start.Floor = AllocateVariable(2)
Trapezium.End.Ceiling = AllocateVariable(2)
Trapezium.End.Floor = AllocateVariable(2)

Start.X = AllocateVariable(2)
Start.Y = AllocateVariable(2)

End.X = AllocateVariable(2)
End.Y = AllocateVariable(2)

Delta.X = AllocateVariable(2)
Delta.AbsX = AllocateVariable(2)

Delta.Y = AllocateVariable(2)
Delta.AbsY = AllocateVariable(2)

Gradient = AllocateVariable(2)

UpperLower.FrontCeilingHeight = AllocateVariable(2)
UpperLower.BackCeilingHeight = AllocateVariable(2)
UpperLower.FrontFloorHeight = AllocateVariable(2)
UpperLower.BackFloorHeight = AllocateVariable(2)

HorizontalEdge.Start.Y = AllocateVariable(2)
HorizontalEdge.End.Y = AllocateVariable(2)

Angle = AllocateVariable(1)
Angle.Transformed = AllocateVariable(1)

.endmodule

.module Tree

Walker.X = AllocateVariable(2)
Walker.Y = AllocateVariable(2)

.endmodule

.module Line

Error = AllocateVariable(1)

ClipPixel = AllocateVariable(2)
Delta.X = AllocateVariable(1)

.endmodule

Level.StructurePointers = AllocateVariable(0)
Level.Vertices = AllocateVariable(2)
Level.Vertices.Count = AllocateVariable(2)
Level.Walls = AllocateVariable(2)
Level.Walls.Count = AllocateVariable(2)
Level.SubSectors = AllocateVariable(2)
Level.Sectors = AllocateVariable(2)
Level.Tree = AllocateVariable(2)
Level.BlockMap = AllocateVariable(2)
Level.StructurePointers.Size = AllocateVariable(0) - Level.StructurePointers

Level.DynamicMemory = AllocateVariable(2)
Level.TransformedVertices = AllocateVariable(2)
Level.AllocatedMemory = AllocateVariable(2)

.module Things

SubSectorStack.Top = AllocateVariable(2)
SubSectorStack.Current = AllocateVariable(2)
SubSectorStack.MaximumCapacity = AllocateVariable(2)
SubSectorStack.EntriesFree = AllocateVariable(2)

SubSectorStack.EntrySize = (96 * 2) + 2

DrawingSubSector = AllocateVariable(2)

Transformed.X = AllocateVariable(2)
Transformed.Y = AllocateVariable(2)

Projected.X = AllocateVariable(1)

Projected.Width = AllocateVariable(1)
Projected.Height = AllocateVariable(1)

Appearance.Offset = AllocateVariable(2)

Appearance = AllocateVariable(0)
Appearance.WorldWidth = AllocateVariable(2)
Appearance.WorldHeight = AllocateVariable(2)
Appearance.SpriteWidth = AllocateVariable(1)
Appearance.SpriteHeight = AllocateVariable(1)
Appearance.ColumnStride = AllocateVariable(2)
Appearance.Size = AllocateVariable(0) - Appearance

.endmodule

.module Sprite

Data = AllocateVariable(2)

Column.Bottom = AllocateVariable(2)
Column.Bottom.Clipped = AllocateVariable(1)

Column.Top = AllocateVariable(2)
Column.Top.Clipped = AllocateVariable(1)

.endmodule

.module Physics

Actor.StartPosition.X = AllocateVariable(2)
Actor.StartPosition.Y = AllocateVariable(2)

Actor.EndPosition.X = AllocateVariable(2)
Actor.EndPosition.Y = AllocateVariable(2)

Actor.Z.Head = AllocateVariable(2)
Actor.Z.Knees = AllocateVariable(2)
Actor.Z.Feet = AllocateVariable(2)

Collision.U = AllocateVariable(2)
Collision.X = AllocateVariable(2)
Collision.Y = AllocateVariable(2)

.endmodule

VariableRegion = VariableRegion2 \ VariableSpace = 160

.module BlockMap

Header = AllocateVariable(0)
Origin.X = AllocateVariable(2)
Origin.Y = AllocateVariable(2)
Width = AllocateVariable(1)
Height = AllocateVariable(1)
Header.Size = AllocateVariable(0) - Header

Grid = AllocateVariable(2)

.endmodule

.module Key

PreviousScancode = AllocateVariable(1)

.endmodule