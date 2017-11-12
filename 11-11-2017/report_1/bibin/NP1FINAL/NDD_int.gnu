set terminal pngcairo font "arial,10" fontscale 1.0 size 1280, 720
set output 'NDD_plot_int.png'
set title "Newton's divided difference"
f(x)=(1/(1+25*x*x))
set xlabel 'X'
set ylabel 'Y'
set grid
set zeroaxis
plot 'Output_NDD.dat' using 1:2 title "Poly" w l ls 6 lc 1, f(x) title "func" w l ls 6 lc 3
