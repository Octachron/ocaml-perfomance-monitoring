set title "Nontypechecking times"
set xtics rotate by -90
set output "witness.pdf"
set term pdf
set bars 0
set grid y my

set xrange [0:2100]
set yrange [0.5:2.0]

plot "by_files.data" u ($0):(($11)/$8) w p pt 0 title "Nontypechecking time/ average typechecking time before"

#plot "by_files.data" u ($0):(($8)/$6):(($9)/$6) w yerrorbars pt 0 title "Nontypechecking time/ average typechecking time before"
