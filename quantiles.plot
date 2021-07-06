set title "Quantiles for typechecking times"
set xtics rotate by -90
set output "quantile.pdf"
set term pdf

# set logscale y 10
# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)

set grid y my

plot "quantiles.data" u 1:2 t "Quantiles" w l
