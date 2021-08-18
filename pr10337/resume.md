# PR 10337 Effect on compilation time  of OCaml programs

To measure the effect of  [#10337](https://github.com/ocaml/ocaml/pull/10337) over the compilation time of OCaml programs.
I have recently worked on a version of the OCaml compiler that can  output the timing information of the compilation
to a specific directory ().
With this change, installing an opam package with
```bash
OCAMLPARAM=",_,timings=1,dump-dir= /tmp/pkgnname" opam install pkgname
```
outputs all profiling information to `/tmp/pkgname`.

This makes it possible to collect large number of datapoints on compilation times by using opam installation process
without the need of much glue.

For the sake of this experiment, I started with 5 core packages `containers`, `dune`, `tyxml`, `coq` and `base`.
Once their dependencies are added, we end up with

- ocamlfind
- num
- zarith
- seq
- containers
- coq
- dune
- re
- ocamlbuild
- uchar
- topkg
- uutf
- tyxml
- sexplib0
- base

Then it is a matter to repeatedly install those packages, and measure the compilation times before and after   [#10337](https://github.com/ocaml/ocaml/pull/10337).

In order to get more reliable statistics on each file, each package was compiled 250 times leading
to 1,6 millions of data points (available at https://www.polychoron.fr/static/longer_complex.log.xz) after
slightly more than a week-end of computation.

In order to try to reduce the noise induced by the operating system scheduler, the compilation process was
run with `OPAMJOBS=1`. Similarly, the compilation process was isolated as much as possible from the other
process using the `cset` Linux utility to reserve one full physical core to the physical process.



## Comparing averages, files by files

With the data hand, we can compute the average compilation by files, and by stage of the ocaml compiler pipeline.
In our case we are mostly interested in the typechecking stage, and global compilation time, since #10337 should only
alters the typechecking times. It is therefore useful to split the compilation time into `typechecking + other=total`.
Then for each files in the 15 packages above we can can compute the average time for each of those stages and the
relative change of average compilation time: `time after/time before`.

Rendering those relative changes for the typechecking time, file by file (with the corresponding 90% confidend interval) yields 

![Relative change in average typechecking time by files](mean_ratio.svg)

In the graph above, there are few remarkable points:

- As expected, the average typechecking time increased for almost all files
- A significant portion of points are stuck to the line "after/before=1". This means that
for those files there was no changes at all of the typechecking times.
- The standard deviation time varies wildly across packages. The typechecking of coq files tend to have a
  very high variances. However outside of those files, the standard deviation seems moderate, and the
  mean estimator seem to have converged.
- We have a handful a files for which the typechecking time more than doubled. However the relative typechecking time
  does seem to be confined in the `[1,1.2]` range for a majority of files.
  
Since the data is quite noisy, it is useful before trying to interpret it to check that we are not looking only at noise.
Fortunately, we have the data on the time spent outside of the typechecking stage available, and we know that
those times should be mostly noise. We have thus a baseline, that looks like

![Relative change in average non-typechecking time by files](other_ratio.svg)


This cloud of points look indeed much noiser. More importantly, it seems centered around the line `after/before=1`.
This means that our hypothesis that the compilation time outside of the typechecking stage has not been altered
is not visibly invalidated by our data points. An other interesting point is that the high variance points seems to be
shared between the typechecking and other graphs.

We can even checks on the graphs for the average total compilatio (files by files)

![Relative change in average total time by files](total_ratio.svg)

that those points still have a high variances here. However, outside of this cluster of points, we have a quite more
compact distribution of points for the total compilation time: it seems that we have a quite consistent increase of the
total compilation time of around 3%.

And this is reflected in the averages:

| Typechecking average | Other average  | Total average |
|----------------------|----------------|---------------|
| 1.06658              | 1.01677        |   1.03288     |

We have thus an increase of around 6.7% of typechecking time which translates to an increase of 3.3% of total time.
However, the non-typechecking time also increased by 1.7% in average. Our average is thus probably tainted by
some structural bias, and we probably cannot count on a precision of more than 1.7% . Even with this caveat,
we still have a visible effect on the total compilation time.

We might better served bt comparing the geometric average. Indeed, we are comparing ratio of time, with possibly a heavy-tailed
noise. By using the geometric average (which compute the exponential of the arithmetic mean of the logarithms of our ratio), we can
check that rare events don't have an indue influence on the average. In our case the geometric means looks like

| Typechecking geometric average| Other  geometric average  | Total  geometric average   |
|-------------------------------|---------------------------|----------------------------|
| 1.05977                       | 1.0136                    | 1.03193                    |

All geometric averages have decreased compared to the arithmetic means, which is a sign that the compilation
time distribution is skewed towards high compilation times. However, the changes are small and do not
alter our previous interpretation.


We can somewhat refine those observations by looking at the medians (which are even less affected by the heavy-tailness of distributions)

| Typechecking median | Other median | Total media |
|---------------------|--------------|-------------|
| 1.03843             | 1.0085       |  1.02506    |

Here, the non-typechecking times seems far less affected by the structural bias (with an increase of 0.8%) whereas the increase
of typechecking time and total compilation time are reduced but still here at 3.8% and 2.5% respectively.


## Comparing averages, quantiles


We can refine our analysis by looking at the quantiles of this relative changes of compilation time

![Quantiles of the relative change of average typechecking time by files](mean_quantiles.svg)
 | % |  mean quantiles      |
|---|--------------------|
| 1% | 0.875097 |
| 10% | 1 |
| 25% | 1.001 |
| 50% | 1.03843 |
| 75% | 1.08837 |
| 90% | 1.163 |
| 99% | 1.51411 |
| 99.9% | 2.76834 |

Here we see that the typechecking time of around 25% of files is simply not affected at all by the changes.
And for half of the files, the compilation time is inferior to 9%. Contrarily, there is 1% of files for which
the typechecking time increases by more than 50% (with outliers around 200%-400% increase).

However, looking at the total compilation does seems to reduce the overall impact of the change

[Quantiles of the relative change in average total time by files](total_quantiles.svg)
| % |  total quantiles      |
|---|--------------------|
| 1% | 0.944124 |
| 10% | 1 |
| 25% | 1.00693 |
| 50% | 1.02506 |
| 75% | 1.05 |
| 90% | 1.07895 |
| 99% | 1.17846 |
| 99.9% | 1.4379 |

Indeed, we still have 25% of files not impacted, but for 65% of files the relative increase of compilation time
is less than 8%. (and the outliers stalls at a 50% increase)

We can also have a quick look at the quantiles for the non-typechecking time 

![Quantiles of the relative change in average non-typechecking time by files](other_quantiles.svg) 
| % |  other quantiles      |
|---|--------------------|
| 1% | 0.837892 |
| 10% | 0.955752 |
| 25% | 0.994896 |
| 50% | 1.0085 |
| 75% | 1.03743 |
| 90% | 1.08618 |
| 99% | 1.25541 |
| 99.9% | 1.67784 |

but here the only curiosity if that the curve is more symmetric and we have 25% of files for which the non-typechecking compilation time
decrease randonmy.


## Noise models and minima

One issue with our previous analysis is this structural bias that we measure in the non-typechecking average times across files.
A possibility to mitigate this issue is to change our noise model. Using an average, we implicitly assumed that the compilation time was
mostly:

```
observable_compilation_time = theoretical_computation_time + noise
```
where noise is a random variable with at least a finite variance and a mean of `0`. Indeed, with this symmetry hypothesis
the expectation of the observable computation time aligns with the theoretical compilation time:
```
E[observable_computation_time] = E[theoretical_computation_time] + E[noise] = theoretical_computation_time
```
Similarly, the variance hypothesis ensure that the empirical average hypothesis converges relatively well towards the theoretical expectation.
However, we can imagine another noise model with a multiplicative noise (due to CPU scheduling for instance),
```
observable_compilation_time = scheduling_noise * theoretical_computation_time + noise
```
with both ` scheduling_noise>1` and `noise>1`.  With this model, the expectation of the observable compilation time does not match up with
the theoretical computation time:
```
E[observable_computation_time] - theoretical_computation_time  = E[(scheduling_noise - 1) * theoretical_computation_time] + E[noise] = 
    (E[scheduling_noise]-1) * theoretical_computation_time +  E[noise]
```
Thus, in this model, the average observable computation time is a structurally biased estimator for the theoretical computation time.
And the behavior that we observed for the non-typechecking time is not in contradiction with this model.

However, the positivity of the noise opens another avenue for estimators: we can consider the minima of a series of independants realization.
Then, we have
```
min(observable_compilation_time) = min(scheduling_noise * theoretical_computation_time) + min(noise) = theoretical_computation_time
```
if the `scheduling_noise` is the left-bounds of the essential support of the `scheduling_noise` and `noise` are `1` and `0` respectively.


## Comparing minima

We can then restart out analysis using the minimal compilation time file-by-file.

![Relative change in minimal typechecking time by files](min_ratio.svg)


## Comparing minima, quantiles




## Compilation profile


![Relative time spent in typechekhing  by files](profile_ratio.svg)



## Conclusion

## Appendices



### Min-based cloud points

![Relative change in minimal non-typechecking time by files](min_other_ratio.svg)
![Relative change in minimal total time by files](min_total_ratio.svg)



### Mean-based quantiles

![Quantiles of the relative time spent in typechekhing  by files](profile_quantiles.svg)


### Min-based quantiles

![Quantiles of the relative change in minimal typechecking time by files](min_quantiles.svg)
![Quantiles of the relative change in minimal non-typechecking time by files](min_other_quantiles.svg)
![Quantiles of the relative change in minimal total time by files](min_total_quantiles.svg)


### Mean-based histograms

![Histogram of the relative change of average typechecking time by files](mean_hist.svg)
![Histogram of the relative change in average non-typechecking time by files](other_hist.svg)
![Histogram of the relative change in average total time by files](total_hist.svg)
![Histogram of the elative time spent in typechekhing  by files](profile_hist.svg)


### Min-based histograms

![Histogram of the relative change in minimal typechecking time by files](min_hist.svg)
![Histogram of the relative change in minimal non-typechecking time by files](min_other_hist.svg)
![Histogram of the relative change in minimal total time by files](min_total_hist.svg)




### Mean-based quantile tables


| % |  profile quantiles      |
|---|--------------------|
| 1% | 0.111556 |
| 10% | 0.165447 |
| 25% | 0.283435 |
| 50% | 0.397314 |
| 75% | 0.488415 |
| 90% | 0.57464 |
| 99% | 0.756093 |
| 99.9% | 0.920419 |

### Min based quantile tables

| % |  min_other quantiles      |
|---|--------------------|
| 1% | 0.8 |
| 10% | 0.944444 |
| 25% | 1 |
| 50% | 1 |
| 75% | 1 |
| 90% | 1.11111 |
| 99% | 1.33333 |
| 99.9% | 1.6 |

| % |  min_profile quantiles      |
|---|--------------------|
| 1% | 0.0588235 |
| 10% | 0.125 |
| 25% | 0.2 |
| 50% | 0.285714 |
| 75% | 0.36 |
| 90% | 0.428571 |
| 99% | 0.666667 |
| 99.9% | 0.802721 |

| % |  min quantiles      |
|---|--------------------|
| 1% | 1 |
| 10% | 1 |
| 25% | 1 |
| 50% | 1 |
| 75% | 1.07692 |
| 90% | 1.2 |
| 99% | 2 |
| 99.9% | 2 |

| % |  min_total quantiles      |
|---|--------------------|
| 1% | 0.909091 |
| 10% | 1 |
| 25% | 1 |
| 50% | 1 |
| 75% | 1.04545 |
| 90% | 1.1 |
| 99% | 1.22727 |
| 99.9% | 1.4 |

