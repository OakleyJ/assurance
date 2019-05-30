README
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# assurance

<!-- badges: start -->

<!-- badges: end -->

An R package for implementing the assurance method for normally
distributed data, with elicited distributions for the treatment effect,
and the population variances in the treatment and control arms. The
package implements the methodology from

  - Alhussain, Z. A. and Oakley, J. E. (2019). [Assurance for clinical
    trial design with normally distributed outcomes: eliciting
    uncertainty about variances](https://arxiv.org/abs/1702.00978v2).
    arXiv:1702.00978v2.

## Installation

The `assurance` package is currently on GitHub only, and requires the
development version of `SHELF` (also on GitHub only). Install these with
the commands

``` r
install.packages("devtools")
devtools::install_github("OakleyJ/SHELF")
devtools::install_github("OakleyJ/assurance")
```

## Example

This code illustrates the example from Alhussein and Oakley (2019).

We suppose the expert judges   
![Pr(\\delta = 0)
= 0.5,](https://latex.codecogs.com/png.latex?Pr%28%5Cdelta%20%3D%200%29%20%3D%200.5%2C
"Pr(\\delta = 0) = 0.5,")  
and conditional on ![\\delta
\\neq 0](https://latex.codecogs.com/png.latex?%5Cdelta%20%5Cneq%200
"\\delta \\neq 0"), the expert judges   
![
Pr(\\delta \\le 0.25|\\delta \\neq 0) = 0.25,
](https://latex.codecogs.com/png.latex?%0APr%28%5Cdelta%20%5Cle%200.25%7C%5Cdelta%20%5Cneq%200%29%20%3D%200.25%2C%0A
"
Pr(\\delta \\le 0.25|\\delta \\neq 0) = 0.25,
")  
  
![
Pr(\\delta \\le 0.4|\\delta \\neq 0) = 0.5, 
](https://latex.codecogs.com/png.latex?%0APr%28%5Cdelta%20%5Cle%200.4%7C%5Cdelta%20%5Cneq%200%29%20%3D%200.5%2C%20%0A
"
Pr(\\delta \\le 0.4|\\delta \\neq 0) = 0.5, 
")  
  
![
Pr(\\delta \\le 0.55|\\delta \\neq 0) = 0.75. 
](https://latex.codecogs.com/png.latex?%0APr%28%5Cdelta%20%5Cle%200.55%7C%5Cdelta%20%5Cneq%200%29%20%3D%200.75.%20%0A
"
Pr(\\delta \\le 0.55|\\delta \\neq 0) = 0.75. 
")  

We fit a distribution to these judgements with the command

``` r
v <- c(0.25, 0.4, 0.55)
p <- c(0.25, 0.5, 0.75)
myfitDelta <- SHELF::fitdist(vals = v, probs = p)
```

We suppose that a response of at least 0.2 is required for a patient to
benefit. We suppose that if treatment effect were ![\\delta
= 0.4](https://latex.codecogs.com/png.latex?%5Cdelta%20%3D%200.4
"\\delta = 0.4"), expert judges between 60% and 80% of patients would
benefit.

``` r
myfitTau <- SHELF::fitprecision(c(-Inf, 0.2),
                                propvals = c(0.2, 0.4),
                                propprobs = c(0.05, 0.95),
                                med = 0.4)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

We now estimate the assurance, for sample sizes of 20 patients per
group:

``` r
assurance::assuranceNormal(fitDelta = myfitDelta,
                fitPrecisionTmt = myfitTau,
                pDeltaZero  = 0.5,
                nTreatment = 20,
                nControl = 20)
#> [1] 0.357
```

Next, we consider the information that might result from observing 10
patients in each arm. Denoting these 20 observations by
![D](https://latex.codecogs.com/png.latex?D "D"), we consider the
predictive distribution of

  
![
Pr(\\delta \> 0 | D),
](https://latex.codecogs.com/png.latex?%0APr%28%5Cdelta%20%3E%200%20%7C%20D%29%2C%0A
"
Pr(\\delta \> 0 | D),
")  
as a function of the yet-to-be-observed data
![D](https://latex.codecogs.com/png.latex?D "D"). The following command
will simulate 1000 trials, and estimate the probability of a positive
treatment effect given the data from each trial.

``` r
pEffective <- assurance::sampleInterim(fitDelta = myfitDelta,
                                       fitPrecisionTmt = myfitTau,
                                       nControl = 10,
                                       nTreatment = 10,
                                       pDeltaZero = 0.5,
                                       nTrials = 1000)
```

We would like   
![
Pr(\\delta \> 0 | D),
](https://latex.codecogs.com/png.latex?%0APr%28%5Cdelta%20%3E%200%20%7C%20D%29%2C%0A
"
Pr(\\delta \> 0 | D),
")  
to be close to 0 or 1. We count the proportion of the 1000 trials in
which this probability is either less than 0.05, or greater than 0.95:

``` r
mean(pEffective < 0.05 | pEffective > 0.95)
#> [1] 0.476
```

We display the predictive distribution of posterior probabilities with a
histogram:

``` r
hist(pEffective, breaks = seq(from = 0, 
                              to = 1, 
                              by = 0.05),
     xlab = expression(P(delta>0~"| trial data")),
     main ="")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## `shiny` app

A `shiny` app is included in the package for implementing all the above
methods. To launch the app, run the command

``` r
assuranceNormalApp()
```
