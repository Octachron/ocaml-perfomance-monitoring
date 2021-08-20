# Mesuring the effect of pull requets on the compilation time of OCaml programs

The OCaml typechecker is an important piece of the OCaml compiler pipeline which accounts for
a significant portion of time spent on compiling an OCaml program (see the [appendices](Compilation-profile)).

The code of the typechecker is also quite optimised, sometimes to the detriment of the readability of the code.
Recently, Jacques Garrigue and Takafumi Saikawa have worked on a series of pull requests to improve the readability
of the typechecker
([#10337](https://github.com/ocaml/ocaml/pull/10337), [#10474](https://github.com/ocaml/ocaml/pull/10474), [#10541](https://github.com/ocaml/ocaml/pull/10541)). Unfortunately, those improvements are also expected
to increase the typechecking time of OCaml programs because they add abstraction barriers, and remove some
abstraction breaking optimisations.

The effect was particularly pronounced on [#10337](https://github.com/ocaml/ocaml/pull/10337). Due to
the improvement of the readability of the typechecker, this pull request has been merged after some quick
tests to check that the compilation time increase was not too dire.

However, the discussion on this pull request highlighted the fact that it was difficult to measure OCaml compilation
time on a scale large enough to enable good statistical analysis.

Consequently, I decided to try my hand at a statistical analysis of OCaml compilation time, using this pull request
 [#10337](https://github.com/ocaml/ocaml/pull/10337) as a case study.
 
Before doing any kind of analysis, I need an easy way to collect the data of interest. 
Fortunately, the OCaml compiler can emit timing information with flag `-dtimings`.
However, this information is emitted on stdout, whereas my ideal sampling process would be to just pick an opam package,
launch a build process and recover the timing information for each file.

This doesn't work if the data is sent to the stdout, and never see again.
The first step is thus to create a version of the OCaml compiler that can output the timing information of the compilation
to a specific directory
With this change ([#10575](https://github.com/ocaml/ocaml/pull/10337)), installing an opam package with
```bash
OCAMLPARAM=",_,timings=1,dump-dir= /tmp/pkgnname" opam install pkgname
```
outputs all profiling information to `/tmp/pkgname`.

This makes it possible to collect large number of data points on compilation times by using opam installation process
without the need of much glue code.

For this case study, I am using 5 core packages `containers`, `dune`, `tyxml`, `coq` and `base`.
Once their dependencies are added, I end up with

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

Then it is a matter of repeatedly installing those packages, and measuring the compilation times before and after  [#10337](https://github.com/ocaml/ocaml/pull/10337).

In order to get more reliable statistics on each file, each package was compiled 250 times leading
to 1,6 millions of data points (available at https://www.polychoron.fr/static/longer_complex.log.xz) after
slightly more than a week-end of computation.

In order to try to reduce the noise induced by the operating system scheduler, the compilation process was
run with `OPAMJOBS=1`. Similarly, the compilation process was isolated as much as possible from the other
process using the `cset` Linux utility to reserve one full physical core to the opam processes.


## Comparing averages, files by files

With the data hand, we can compute the average compilation by files, and by stage of the OCaml compiler pipeline.
In our case, we are mostly interested in the typechecking stage, and global compilation time, since #10337 should only
alters the typechecking times. It is therefore useful to split the compilation time into `typechecking + other=total`.
Then for each files in the 15 packages above, I can can compute the average time for each of those stages and the
relative change of average compilation time: `time after/time before`.

Rendering those relative changes for the typechecking time, file by file (with the corresponding 90% confidence interval) yields 

![Relative change in average typechecking time by files](mean_ratio.svg)

To avoid noise, I have removed files for which the average typechecking time was inferior to one microsecond on
the reference version of the compiler.

In the graph above, there are few remarkable points:

- As expected, the average typechecking time increased for almost all files
- A significant portion of points are stuck to the line "after/before=1". This means that
for those files there was no changes at all of the typechecking times.
- The standard deviation time varies wildly across packages. The typechecking of some dune files tend to have a
  very high variances. However outside of those files, the standard deviation seems moderate, and the
  mean estimator seem to have converged.
- For a handful a files for which the typechecking time more than doubled. However the relative typechecking time
  does seem to be confined in the `[1,1.2]` range for a majority of files.
  
Since the data is quite noisy, it is useful before trying to interpret it to check that we are not looking only at noise.
Fortunately, we have the data on the time spent outside of the typechecking stage available, and
those times should be mostly noise. We have thus a baseline, that looks like

![Relative change in average non-typechecking time by files](other_ratio.svg)


This cloud of points look indeed much noisier. More importantly, it seems centred around the line `after/before=1`.
This means that our hypothesis that the compilation time outside of the typechecking stage has not been altered
is not visibly invalidated by our data points. An other interesting point is that the high variance points seems to be
shared between the typechecking and other graphs.

We can even checks on the graphs for the average total compilation (files by files)

![Relative change in average total time by files](total_ratio.svg)

that those points still have a high variances here. However, outside of this cluster of points, we have a quite more
compact distribution of points for the total compilation time: it seems that we have a quite consistent increase of the
total compilation time of around 3%.

And this is reflected in the averages:

| Typechecking average | Other average  | Total average |
|----------------------|----------------|---------------|
| 1.06641              | 1.01756        |   1.03307     |

We have thus an increase of around 6.6% of typechecking time which translates to an increase of 3.3% of total time.
However, the non-typechecking time also increased by 1.7% in average. Our average is thus either tainted by
some structural bias or the relative variance (mean/ratio) is still enough for the distribution of the ratio to be ill-behaved
(literature seems to indicate that a relative variance < 10% is required for the distribution of ratio to be Gaussian-like).
Anyway, we probably cannot count on a precision of more than 1.7%.
Even with this caveat, we still have a visible effect on the total compilation time.

We might better served by comparing the geometric average. Indeed, we are comparing ratio of time, with possibly a heavy-tailed
noise. By using the geometric average (which compute the exponential of the arithmetic mean of the logarithms of our ratio), we can
check that rare events don't have an undue influence on the average. In our case the geometric means looks like

| Typechecking geometric average| Other geometric average  | Total geometric average   |
|-------------------------------|--------------------------|---------------------------|
| 1.05963                       | 1.01513                  | 1.03215                   |

All geometric averages have decreased compared to the arithmetic means, which is a sign that the compilation
time distribution is skewed towards high compilation times. However, the changes are small and do not
alter our previous interpretation.


We can somewhat refine those observations by looking at the medians (which are even less affected by the heavy-tailness of distributions)

| Typechecking median | Other median | Total media |
|---------------------|--------------|-------------|
| 1.03834             | 1.00852      |  1.02507    |

Here, the non-typechecking times seems far less affected by the structural bias (with an increase of 0.9%) whereas the increase
of typechecking time and total compilation time are reduced but still here at 3.8% and 2.5% respectively.


## Comparing averages, quantiles


We can refine our analysis by looking at the quantiles of this relative changes of compilation time

![Quantiles of the relative change of average typechecking time by files](mean_quantiles.svg)

| %     | mean quantiles |
|-------|----------------|
| 1%    | 0.875097       |
| 10%   | 1              |
| 25%   | 1.001          |
| 50%   | 1.03834        |
| 75%   | 1.08826        |
| 90%   | 1.162          |
| 99%   | 1.51411        |
| 99.9% | 2.76834        |

Here we see that the typechecking time of around 25% of files is simply not affected at all by the changes.
And for half of the files, the compilation time is inferior to 9%. Contrarily, there is 1% of files for which
the typechecking time increases by more than 50% (with outliers around 200%-400% increase).

However, looking at the total compilation does seems to reduce the overall impact of the change

[Quantiles of the relative change in average total time by files](total_quantiles.svg)

| %     | total quantiles |
|-------|-----------------|
| 1%    | 0.945555        |
| 10%   | 1               |
| 25%   | 1.00707         |
| 50%   | 1.02507         |
| 75%   | 1.05            |
| 90%   | 1.07895         |
| 99%   | 1.17846         |
| 99.9% | 1.4379          |

Indeed, we still have 25% of files not impacted, but for 65% of files the relative increase of compilation time
is less than 8%. (and the outliers stalls at a 50% increase)

We can also have a quick look at the quantiles for the non-typechecking time 

![Quantiles of the relative change in average non-typechecking time by files](other_quantiles.svg) 

| %     | other quantiles |
|-------|-----------------|
| 1%    | 0.855129        |
| 10%   | 0.956174        |
| 25%   | 0.995239        |
| 50%   | 1.00852         |
| 75%   | 1.03743         |
| 90%   | 1.08618         |
| 99%   | 1.25541         |
| 99.9% | 1.67784         |

but here the only curiosity if that the curve is more symmetric and we have 25% of files for which the non-typechecking compilation time
decrease randomly.


## Noise models and minima

One issue with our previous analysis is the high variance which is observable in the non-typechecking average times across files.
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
and the variance `Var[observable_computation_time]` is exactly the variance of the noise `Var[noise]`.
Then our finite variance hypothesis ensure that the empirical average hypothesis converges relatively well towards the theoretical expectation.


However, we can imagine another noise model with a multiplicative noise (due to CPU scheduling for instance),
```
observable_compilation_time = scheduling_noise * theoretical_computation_time + noise
```
with both ` scheduling_noise>1` and `noise>1`. With this model, the expectation of the observable compilation time does not match up with
the theoretical computation time:
```
E[observable_computation_time] - theoretical_computation_time =
  (E[scheduling_noise]-1) * theoretical_computation_time + E[noise]
```
Thus, in this model, the average observable computation time is a structurally biased estimator for the theoretical computation time.
This bias might be compensated by the fact that we are only looking to ratio.
Nevertheless, this model also induces a second source of variance
```
Var[observable_computation_time] = theoretical_computation_time^2 Var[scheduling_noise] + Var[noise]
```
(assuming the two noises are not correlated), and this variance increases with the theoretical computation time.
This relative standard deviation might be problematic when computing ratio.

If this second noise model is closer to reality, using the empirical average estimators might be not ideal.
However, the positivity of the noise opens another avenue for estimators: we can consider the minima of a series of independent realisations.
Then, we have
```
min(observable_compilation_time) = min(scheduling_noise * theoretical_computation_time) + min(noise) = theoretical_computation_time
```
if the `min(scheduling_noise)=1` and `min(noise)=0`.

This model has another advantage: by assuming that the (essential) support of the noise distribution has finite lower bound, we know
that the empirical minima will converge towards a three-parameter Weibull distribution with a strictly positive support.
(To be completely explicit, we also need to assume some regularity of the distribution around this lower bound too).

This means that the distribution ratio of the empirical minima will not exhibits the infinite moments of the ratio of two Gaussians.
Without this issue, our estimator should have less variance.

However, we cannot use Gaussian confidence intervals for the empirical minima. Moreover, estimating the confidence interval for the
Weibull distribution is more complex. Since we are mostly interested in corroborating our previous result, we are bypassing the
computation of those confidence intervals.


## Comparing minima

We can then restart out analysis using the minimal compilation time file-by-file.
Starting with the minimal typechecking time, we get

![Relative change in minimal typechecking time by files](min_ratio.svg)

There are notable differences with the average version:
- a very significant part of our points takes the same time to typecheck before and after #10337
- there is a discretization effects going on: data points tend to fall on exactly the same value of the ratio

Beyond those changes, there is still a visible general increase of typechecking time.

The same differences are visible for the non typechecking compilation time
![Relative change in minimal non-typechecking time by files](min_other_ratio.svg)

and the total compilation time

![Relative change in minimal total time by files](min_total_ratio.svg)

but overall the minimal total compilation and non-typechecking time mirrors what we had seen
with the average. The distribution of the non-typechecking times is maybe more evenly centred around a ratio of 1.

We can have a look at the averages and median (across files) to have more global point of view

  |                    | Typechecking | Other   | Total   |
  |--------------------|--------------|---------|---------| 
  | Average            |  1.06907     | 1.01031 | 1.02901 |
  | Geometric average  |  1.05998     | 1.00672 | 1.0276  |
  | Median             |  1           | 1       | 1       |

A striking change is that the median for the typechecking and total compilation time is equal to one:
more than half of files are not affected by the changes when looking at the minimal compilation time.
This might be an issue with the granularity of time measurement, or it could be a genuine fact.

More usefully, we still have an increase of average typechecking time between 6% and 6.9% depending on the averaging methods, which 
translates to a total compilation time increase between 2.7% and 3.3%. And this time, the increase of unrelated compilation time
is between 0.7% to 0.9%. This seems to confirms that do have a real increase of average compilation time and 3% increase time is a reasonable
number.


## Comparing minima, quantiles

With the discretization, the quantiles of the compilation time are quite interesting and uncharacteristic.

For instance the typechecking quantiles,

![Quantiles of the relative change in minimal typechecking time by files](min_quantiles.svg)

| %     | min quantiles |
|-------|---------------|
| 1%    | 1             |
| 10%   | 1             |
| 25%   | 1             |
| 50%   | 1             |
| 75%   | 1.07692       |
| 90%   | 1.2           |
| 99%   | 2             |
| 99.9% | 2             |


are stuck to 1 between the first and 50th centile. In other words the minimal typechecking time of more than 50% of the files
in our experiment is unchanged. For 40% of the files, the increase is less than 20%. And the most extreme files see only
an increase of 100% of the typechecking time. On the higher quantiles, the presence of multiple jumps is the consequence of
the discretization of ratio that was already visible on the raw data. 


When looking at the time spent outside of typechecking, 

![Quantiles of the relative change in minimal non-typechecking time by files](min_other_quantiles.svg)

| %     |  min_other quantiles |
|-------|----------------------|
| 1%    | 0.8                  |
| 10%   | 0.947368             |
| 25%   | 1                    |
| 50%   | 1                    |
| 75%   | 1                    |
| 90%   | 1.11111              |
| 99%   | 1.33333              |
| 99.9% | 1.6                  |

we observe that the non-typechecking relation compilation time for more than 80% of file is unaffected by the change (or somehow accelerated for 10% of files).

The quantiles for the total compilation time, 
![Quantiles of the relative change in minimal total time by files](min_total_quantiles.svg)

| %     |  min_total quantiles |
|-------|----------------------|
| 1%    | 0.92                 |
| 10%   | 1                    |
| 25%   | 1                    |
| 50%   | 1                    |
| 75%   | 1.04545              |
| 90%   | 1.1                  |
| 99%   | 1.22727              |
| 99.9% | 1.4                  |

mostly reflects the trends set by the typechecking time: 55% of files are unaffected. For 90% of file the increase is less than
10%, and the maximal impact on the compilation time peaks at a 40% relative increase.


## Conclusion

To sum up, with the available data at hands, it seems sensible to conclude that #10337 resulted in an average increase of
compilation time of the order of 3%, while the average relative increase of typechecking time is around 6%. Moreover,
for the most impacted files (at the ninth decile), the relative increase in compilation time ranges between 10% to 40%.

## Appendices

### Compilation profile


Since we have data for both typechecking time and non-typechecking times for a few thousand files, it is interesting
to check how much time is spent on typechecking. We can start by looking at the data points files by files:

![Relative time spent in typechecking by files](profile_ratio.svg)

We have here a relatively uniform cloud of points between 20-60% of time spent in typechecking compared to total
compilation time. This is is reflected on the average and median

| Arithmetic average  | Median       |
|---------------------|--------------|
| 38.8827%            | 39.7336%     |

Both value are quite comparable, the distribution doesn't seem significantly skewed.

However, we have a clear cluster of files for which typechecking accounts for 90% of the total compilation
time. Interestingly, this cluster of points corresponds to the dune cluster of files with a very variance that we had identified
earlier. This explains why those files have essentially the same profile when looking at the total and typechecking compilation
time: in their case, typechecking accounts for most of the work done during compilation.

This relatively uniform distribution is visible both on the quantiles (with an affine part of the quantiles)

![Quantiles of the relative time spent in typechecking by files](profile_quantiles.svg)

| %     |  profile quantiles |
|-----  |--------------------|
| 1%    | 0.111556           |
| 10%   | 0.16431            |
| 25%   | 0.283249           |
| 50%   | 0.397336           |
| 75%   | 0.487892           |
| 90%   | 0.573355           |
| 99%   | 0.749689           |
| 99.9% | 0.913336           |

and on the histogram of the relative time spent in typechecking

![Histogram of the relative time spent in typechecking by files](profile_hist.svg)


### Histograms 

Histogram versions for the quantile diagrams are also available. Due to the mixture of continuous and discrete distributions
they are not that easy to read. Note that those histograms have equiprobable bins (in other words, constant area) rather than constant width bins.

### Average compilation time histograms

![Histogram of the relative change of average typechecking time by files](mean_hist.svg)
![Histogram of the relative change in average non-typechecking time by files](other_hist.svg)
![Histogram of the relative change in average total time by files](total_hist.svg)

An interesting take-away for those histograms is that the typechecking and total compilation time distribution
are clearly skewed to the right: with very few exceptions, compilation increases. Contrarily the non-typechecking
time distribution is much more symmetric. Since the change here is due to noise, there is no more reason for
the compilation time to increase or decrease.

### Minimal compilation time histograms

There is no much change when looking at the histogram for the minimal compilation time for a file

![Histogram of the relative change in minimal typechecking time by files](min_hist.svg)
![Histogram of the relative change in minimal non-typechecking time by files](min_other_hist.svg)
![Histogram of the relative change in minimal total time by files](min_total_hist.svg)

The most notable difference is that the non-typechecking histogram is completely dominated by the dirac distribution centred at `x=1``.
