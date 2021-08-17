set title "Typechecking times"
set xtics rotate by -90
set output "ratio.pdf"
set term pdf

# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)


set bars 0
set grid y my

set xrange [0:2100]
set yrange [0.5:2.0]

# Typechecking

set output "min_ratio.pdf"

plot "by_files.data" u ($0):($5/$2) w p pt 0 title "Minimal typechecking time: after/before"

set output "mean_ratio.pdf"
plot "by_files.data" u ($0):(($6)/$3):(($7)/$3) w yerrorbars pt 0 title "Average typechecking time: after/before"


#"by_files.data" u ($0):(($4-$5)/$2):(($4+$5)/$2) with filledcurves title "After/before average(90% confidence interval)",\
#"by_files.data" u ($0):(($4+$5)/$2) w p pt 0 title "",\
#"by_files.data" u ($0):(($4-$5)/$2) w p pt 0 title "",\
#by_files.data" u ($0):(($5-$6)/$3):(($3+$5)/$3) with filledcurves title "Variance of the reference timing info (90% confidence interval)"

# Non-typechecking


set output "min_witness_ratio.pdf"
plot "by_files.data" u ($0):($11/$8) w p pt 0 title "Minimal non-typechecking time: after/before"

set output "mean_witness_ratio.pdf"
plot "by_files.data" u ($0):(($12)/$9):(($13)/$9) w yerrorbars pt 0 title "Average non-typechecking time: after/before"


# Total

set output "min_total_ratio.pdf"
plot "by_files.data" u ($0):($17/$14) w p pt 0 title "Total time: after/before"

set output "mean_total_ratio.pdf"
plot "by_files.data" u ($0):(($18)/$15):(($19)/$15) w yerrorbars pt 0 title "Total time: after/before"
