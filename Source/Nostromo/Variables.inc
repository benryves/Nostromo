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