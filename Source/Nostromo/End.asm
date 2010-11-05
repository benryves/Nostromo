.module Nostromo
End:
CompletedColumns = (End + $FF) & $FF00
TopEdgeClip = Nostromo.CompletedColumns + $100
BottomEdgeClip = Nostromo.TopEdgeClip + $100
Maths.Trig.Table = Nostromo.BottomEdgeClip + $100
.include "Variables.inc"
.endmodule