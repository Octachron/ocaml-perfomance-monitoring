set title "Typechecking times"
set xtics rotate by -90
set output "group1.pdf"
set term pdf

# set logscale y 10
# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)

set grid y my

plot "group_1.data" u ($0):(($4-$5)/$2):(($4+$5)/$2) with filledcurves title "After/before average(90% confidence interval)",\
"group_1.data" u ($0):(($2-$3)/$2):(($2+$3)/$2) with filledcurves title "Variance of the reference timing info (90% confidence interval)"
