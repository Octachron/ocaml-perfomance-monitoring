set title "Typechecking times"
set xtics rotate by -90
set output "ratio.pdf"
set term pdf

# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)


set bars 0
set grid y my

set xrange [0:2100]
set yrange [0.5:2.0]
plot "by_files.data" u ($0):(($4)/$2):(($5)/$2) w yerrorbars pt 0 title "Typechecking time/ average typechecking time before"


#"by_files.data" u ($0):(($4-$5)/$2):(($4+$5)/$2) with filledcurves title "After/before average(90% confidence interval)",\
#"by_files.data" u ($0):(($4+$5)/$2) w p pt 0 title "",\
#"by_files.data" u ($0):(($4-$5)/$2) w p pt 0 title "",\
""by_files.data" u ($0):(($2-$3)/$2):(($2+$3)/$2) with filledcurves title "Variance of the reference timing info (90% confidence interval)"
