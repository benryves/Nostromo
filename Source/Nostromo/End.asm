.module Nostromo
Things.SortedSpriteBuffer:
.fill 4 * 8
Things.SortedSpriteBuffer.Count:
.db 0
End:
TopEdgeClip = (End + $FF) & $FF00
BottomEdgeClip = Nostromo.TopEdgeClip + $100
Maths.Trig.Table = Nostromo.BottomEdgeClip + $100
Vertices.AlreadyTransformed = Maths.Trig.Table + $200
.include "Variables.inc"
.endmodule