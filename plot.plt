# plot.plt
set title "Magnetisation vs Temperature"
set nokey
set grid
set xlabel "Temperature"
set ylabel "Magnetisation"
m="OUT.dat"
plot m using 1:2 with linespoints
