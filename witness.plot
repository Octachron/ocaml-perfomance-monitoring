
set title "Non-typechecking times"
set xtics rotate by -90
set output "witness.pdf"
set term pdf

set logscale y 10

set grid y my

plot "by_files.data" u ($0):(($8-$9)/$6):(($8+$9)/$6) with filledcurves title "After/before average(90% confidence interval)",\
"by_files.data" u ($0):(($6-$7)/$6):(($6+$7)/$6) with filledcurves title "Variance of the reference timing info (90% confidence interval)"
# "by_files.data" u ($0):($4/$2) title ""
