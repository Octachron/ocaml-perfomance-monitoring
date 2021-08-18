set title "Quantiles for typechecking times"
set xtics rotate by -90
set term pdf

# set logscale y 10
# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)

set grid y my
set grid x mx

set yrange [0.8:1.6]

set xlabel "Quantiles"
set ylabel "after/before %"

set output "quantiles.pdf"
plot "mean_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for non-typechecking times"
set output "other_quantiles.pdf"
plot "other_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for total times"
set output "total_quantiles.pdf"
plot "total_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for minimal typechecking times"
set output "min_quantiles.pdf"
plot "min_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for minimal nontypechecking times"
set output "min_other_quantiles.pdf"
plot "min_other_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for minimal total times"
set output "min_total_quantiles.pdf"
plot "min_total_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for profile"
set output "min_profile_quantiles.pdf"
plot "min_profile_quantiles.data" u 2:1 t "CDF" w l
