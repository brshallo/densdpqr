
<!-- README.md is generated from README.Rmd. Please edit that file -->

# densdpqr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of densdpqr is to make a minimal package for applying
the`[dpqr]*()` distribution functions (e.g. `dnorm()`, `pnorm()`,
`qnorm()`, `rnorm()`), for any arbitrary output from the
`stats::density()` function.

## Example

1.  Call `stats::density()` on your data to get an estimate for a
    density function.
2.  Call `densdpr::smooth_density()` to smooth your density function and
    create the needed distribution functions for `ddens()`, `pdens()`,
    `qdens()` and `rdens()`.
3.  Pass output from `smooth_density()` and other required parameter
    into `[dpqr]dens()` function(s).

``` r
# devtools::install_github("brshallo/densdpqr")
library(densdpqr)

set.seed(123)
data <- rnorm(10000)
dens <- density(data)
sdens <- smooth_density(dens)

ddens(sdens, 2)
#> [1] 0.05513055
pdens(sdens, 2)
#> [1] 0.9770511
qdens(sdens, 0.977)
#> [1] 1.999054
rdens(sdens, 10L)
#>  [1]  2.3939920 -0.5273786 -0.1777766 -1.0160578  0.9251596 -0.8309481
#>  [7] -0.5785291  0.1021182  0.2127245 -0.1567640
```

# Problems

-   `smooth_density()` uses a separate smoothing spline to build the
    requisite functions for `ddens()`, `pdens()` and `qdens()`. This is
    inefficient *and* means it is possible for the different
    distribution functions to be slightly inconsistent with one another.
    What likely *should* be done is…
    -   `ddens()` should serve as the foundation.
    -   `pdens()` would be the integral of `ddens()`.
    -   `qdens()` would be the inverse of `pdens()`.
    -   This would ensure that all distribution functions are consistent
        with one another[1].
-   May want to change fit of smoother to one that is
    [orthogonal](https://en.wikipedia.org/wiki/Cubic_Hermite_spline)[2]
-   `qdens()` should not accept values outside of 0 and 1, but it
    currently does; `pdens()` should not return values outside of 0 to
    1, but it can.
    -   This could just be hard capped, but it would be preferable to
        use a smoothing function that respected these density
        constraints more intelligently[3].
-   The smoothing method in `smooth_density()` will not necessarily
    respect the `from` and `to` ranges set when creating `density()`[4].
-   Not tested on wide range of distributions.

# Inspiration & Other Resources

Thank you to
[spatstat](https://github.com/spatstat/spatstat.core/blob/76d20a642867d7c30a0f11c58af5b8634ad302cd/R/quantiledensity.R)
authors as well as William Huber whose comment on [How to find
probability density function from density
function…](https://stats.stackexchange.com/a/553271/193123) inspired the
approach in densdpqr.

-   **UPDATE** discovered [pdqr](https://github.com/echasnovski/pdqr)
    after initial share, would probably just use this.
-   [logspline](https://cran.r-project.org/web/packages/logspline/logspline.pdf)
    package
-   [ks](https://cran.r-project.org/web/packages/ks/ks.pdf) package
-   [Nonparametric Statistics, Kernel density
    estimation…](https://bookdown.org/egarpor/NP-UC3M/kde-i.html) online
    book
-   [CRAN Task View: Probability
    Distributions](https://cran.r-project.org/web/views/Distributions.html)

In most simple univariate cases could just use the
`logspline::[dpqr]logspline()` functions which are set-up the same way
as `densdpqr::[dpqr]dens()` but without the same [Problems](#problems).
The advantages with densdpqr are it allows the use of the base
`stats::density()` function, it has fewer dependencies, and is [likely
faster](https://gist.github.com/brshallo/ea2e04347e14fae7ff969a54e2266359).

[1] When I tried to pass `lower = -Inf` into `integrate()` I got errors
that the function was divergent… so would need to change the smoothing
method in some way to prevent this. I also wasn’t sure how to
efficiently return the inverse of `pdens()`.

[2] Current method may be biased towards smoothing density rather than
the inverse, the quantiles.

[3] In the interim, maybe should at least at a `warning()` and just
count on user being thoughtful.

[4] Again are just relying on user to be intelligent when using.
