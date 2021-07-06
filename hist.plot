set title "Typechecking increase histogram"
set output "hist.pdf"
set term pdf

# set logscale y 10
# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)
set xrange [0.9:1.5]
set yrange [0:40000]
set grid y my

plot "hist.data" u (($1+$2)/2):3:(($2-$1)) w boxes t "Typechecking time after PR"
