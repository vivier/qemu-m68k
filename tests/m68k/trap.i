.macro exit value
        move.l #\value,%d1
        move.l #1, %d0
        trap #0
.endm
