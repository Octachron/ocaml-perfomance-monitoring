set xtics rotate by -90
set output "basic.pdf"
set term pdf
plot "basic.data" u ($0):($2-$3):($2+$3):xtic(1) with filledcurves
