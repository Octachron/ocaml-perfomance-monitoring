set title "Nontypechecking increase histogram"
set output "antihist.pdf"
set term pdf


# set logscale y 10
# set ytics (0.5,0.75,1.,1.25,1.5, 2., 4.)
set xrange [0.9:1.5]
set yrange [0:25000]
set grid y my

plot "antihist.data" u (($1+$2)/2):3:(($2-$1)) w boxes t "Nontypechecking time after PR"
