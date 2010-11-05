.function AllocateVariable(size)
	AllocateVariable = VariableRegion
	VariableRegion += size
	VariableSpace -= size
	.if VariableSpace < 0
		.echoln "Insufficient variable space."
	.endif
.endfunction

VariableRegion1 = CompletedColumns + 96

VariableRegion = VariableRegion1 \ VariableSpace = 160

CameraVariables = AllocateVariable(0)
Camera.X = AllocateVariable(2)
Camera.Y = AllocateVariable(2)
Camera.Z = AllocateVariable(2)
Camera.Angle = AllocateVariable(1)
Camera.YShear = AllocateVariable(1)
CameraVariables.Size = AllocateVariable(0) - CameraVariables

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

Projected.Y.Bottom = AllocateVariable(2)
Projected.Y.Bottom.Clipped = AllocateVariable(1)

Projected.Y.Top = AllocateVariable(2)
Projected.Y.Top.Clipped = AllocateVariable(1)

Projected.Width = AllocateVariable(1)
Projected.Height = AllocateVariable(1)

.endmodule