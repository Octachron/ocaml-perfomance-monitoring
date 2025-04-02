set term svg

# set logscale y 10
# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)
set grid y my

set boxwidth 0.9 relative
set style fill solid 0.5

set xlabel "after/before"
set ylabel "probability density"
set yrange [0:17000]
set title "Typechecking increase histogram"
set output "mean_hist.svg"
plot "mean_hist.data" u (($1+$2)/2):3:(($2-$1)) w boxes t ""


set yrange [0:30000]
set title "NonTypechecking increase histogram"
set output "other_hist.svg"
plot "other_hist.data" u (($1+$2)/2):3:(($2-$1)) w boxes t ""

set yrange [0:6500]
set title "Minimal typechecking increase histogram"
set output "min_hist.svg"
plot "min_hist.data" u (($1+$2)/2):3:(($2-$1)) w boxes t ""


set yrange [0:2100]
set title "NonTypechecking increase histogram"
set output "min_other_hist.svg"
plot "min_other_hist.data" u (($1+$2)/2):3:(($2-$1)) w boxes t ""


set yrange [0:12000]
set title "Minimal typechecking increase histogram"
set output "min_total_hist.svg"
plot "min_total_hist.data" u (($1+$2)/2):3:(($2-$1)) w boxes t ""


set yrange [0:30000]
set title "Average typechecking increase histogram"
set output "total_hist.svg"
plot "total_hist.data" u (($1+$2)/2):3:(($2-$1)) w boxes t ""


set yrange [0:6000]
set xlabel "typechecking/total"
set title "Time spent in typechecking histogram"
set output "profile_hist.svg"
plot "profile_hist.data" u (($1+$2)/2):3:(($2-$1)) w boxes t ""
