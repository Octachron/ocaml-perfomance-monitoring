set title "Quantiles for typechecking times"
set xtics rotate by -90
set term svg

# set logscale y 10
# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)

set grid y my
set grid x mx

set yrange [0.8:1.6]

set xlabel "Quantiles"
set ylabel "after/before %"

set output "mean_quantiles.svg"
plot "mean_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for non-typechecking times"
set output "other_quantiles.svg"
plot "other_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for total times"
set output "total_quantiles.svg"
plot "total_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for minimal typechecking times"
set output "min_quantiles.svg"
plot "min_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for minimal nontypechecking times"
set output "min_other_quantiles.svg"
plot "min_other_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for minimal total times"
set output "min_total_quantiles.svg"
plot "min_total_quantiles.data" u 2:1 t "CDF" w l

set title "Quantiles for profile"
set output "profile_quantiles.svg"

set yrange [0.:1.]
plot "profile_quantiles.data" u 2:1 t "CDF" w l
