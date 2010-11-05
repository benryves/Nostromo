VariableRegion1 = CompletedColumns + 96 ; 160 bytes
VariableRegion2 = TopEdgeClip + 96 ; 160 bytes
VariableRegion3 = BottomEdgeClip + 96 ; 160 bytes

VariablePointer = VariableRegion1
VariableSpace = 160

.function AllocateVariable(size)
	AllocateVariable = VariablePointer
	VariablePointer += size
	VariableSpace -= size
	.if VariableSpace < 0
		.echoln "Out of space!"
	.endif
.endfunction

CameraVariables = AllocateVariable(0)
Camera.X = AllocateVariable(2)
Camera.Y = AllocateVariable(2)
Camera.Z = AllocateVariable(2)
Camera.Angle = AllocateVariable(1)
Camera.YShear = AllocateVariable(1)
CameraVariables.Size = AllocateVariable(0) - CameraVariables

ColumnsToDraw = AllocateVariable(1)

Render.Camera.Z = AllocateVariable(2)
Render.Camera.YShear = AllocateVariable(2)

Sector.Front = AllocateVariable(2)
Sector.Back = AllocateVariable(2)

Previous.Camera.X = AllocateVariable(2)
Previous.Camera.Y = AllocateVariable(2)
Previous.Camera.Angle = AllocateVariable(1)

AllocatedTableMemory = AllocateVariable(2)
ClipTableAddress = AllocateVariable(2)

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
Level.AllocatedMemory = AllocateVariable(0)

Vertices.SinA = AllocateVariable(2)
Vertices.CosA = AllocateVariable(2)

Wall.Trapezium.Start.Column = AllocateVariable(1)
Wall.Trapezium.End.Column = AllocateVariable(1)

Wall.Trapezium.Start.Ceiling = AllocateVariable(2)
Wall.Trapezium.Start.Floor = AllocateVariable(2)
Wall.Trapezium.End.Ceiling = AllocateVariable(2)
Wall.Trapezium.End.Floor = AllocateVariable(2)

Wall.Start.X = AllocateVariable(2)
Wall.Start.Y = AllocateVariable(2)

Wall.End.X = AllocateVariable(2)
Wall.End.Y = AllocateVariable(2)

Wall.Delta.X = AllocateVariable(2)
Wall.Delta.AbsX = AllocateVariable(2)

Wall.Delta.Y = AllocateVariable(2)
Wall.Delta.AbsY = AllocateVariable(2)

Wall.Gradient = AllocateVariable(2)

Wall.UpperLower.FrontCeilingHeight = AllocateVariable(2)
Wall.UpperLower.BackCeilingHeight = AllocateVariable(2)
Wall.UpperLower.FrontFloorHeight = AllocateVariable(2)
Wall.UpperLower.BackFloorHeight = AllocateVariable(2)

Wall.HorizontalEdge.Start.Y = AllocateVariable(2)
Wall.HorizontalEdge.End.Y = AllocateVariable(2)

Tree.Walker.X = AllocateVariable(2)
Tree.Walker.Y = AllocateVariable(2)

Things.SubSectorStack.Top = AllocateVariable(2)
Things.SubSectorStack.Current = AllocateVariable(2)
Things.SubSectorStack.MaximumCapacity = AllocateVariable(1)
Things.SubSectorStack.EntriesFree = AllocateVariable(1)

Things.DrawingSubSector = AllocateVariable(2)

Things.Transformed.X = AllocateVariable(2)
Things.Transformed.Y = AllocateVariable(2)

Things.Projected.X = AllocateVariable(1)

Things.Projected.Y.Bottom = AllocateVariable(2)
Things.Projected.Y.Bottom.Clipped = AllocateVariable(1)

Things.Projected.Y.Top = AllocateVariable(2)
Things.Projected.Y.Top.Clipped = AllocateVariable(1)

Things.Projected.Width = AllocateVariable(1)
Things.Projected.Height = AllocateVariable(1)

Line.Error = AllocateVariable(1)

Line.ClipPixel = AllocateVariable(2)
Line.Delta.X = AllocateVariable(1)