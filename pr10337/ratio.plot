set title "Typechecking times"
set xtics rotate by -90
set output "ratio.svg"
set term svg

# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)

set grid x2tics
set x2tics (203,290,1118,1567,1577,1638,1651,1673,1678,1708,1729,1731) format "" scale 0

set xtics ("base" 100,"containers" 250,"coq" 700, "dune" 1300, "ocamlbuild" 1600) format ""

set bars 0
set grid y my

set xrange [0:1737]
set yrange [0.5:2.0]

# Typechecking

set output "min_ratio.svg"

set title "Minimal typechecking time: after/before"
plot "by_files.data" u ($0):($5/$2) w p pt 0 title ""

set output "mean_ratio.svg"
set title "Average typechecking time: after/before"
plot "by_files.data" u ($0):(($6)/$3):(($7)/$3) w yerrorbars pt 0 title ""


# Non-typechecking

set output "min_other_ratio.svg"
set title "Minimal non-typechecking time: after/before"

plot "by_files.data" u ($0):($11/$8) w p pt 0 title ""

set output "other_ratio.svg"
set title "Average non-typechecking time: after/before"
plot "by_files.data" u ($0):(($12)/$9):(($13)/$9) w yerrorbars pt 0 title ""


# Total

set output "min_total_ratio.svg"
set title "Minimal Total time by files"
plot "by_files.data" u ($0):($17/$14) w p pt 0 title ""

set output "total_ratio.svg"
set title "Average total time by files"
plot "by_files.data" u ($0):(($18)/$15):(($19)/$15) w yerrorbars pt 0 title "Average total time: after/before"

# profile

set yrange [0.:1.]


set output "profile_ratio.svg"
plot "by_files.data" u ($0):24:25 w yerrorbars pt 0 title "Relative time spent typechecking"
