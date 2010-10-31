.module Nostromo
End:
CompletedColumns = (End + $FF) & $FF00
TopEdgeClip = Nostromo.CompletedColumns + $100
BottomEdgeClip = Nostromo.CompletedColumns + $200
.endmodule