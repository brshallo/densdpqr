#' Smooth Density
#'
#' Smoothes output of `density()` and returns functions needed to run other
#' functions on the distribution.
#'
#' @param d Density object (returned from running `density()`)
#'
#' @return List of functions `ddens()`, `pdens()`, `qdens()`, `rdens()`
#' @export
#' @examples
#' set.seed(123)
#' data <- rnorm(10000)
#' dens <- density(data)
#'
#' sdens <- smooth_density(dens)
smooth_density <- function(d){

  ddens <- with(d, splinefun(x, y, method = "monoH.FC"))

  pdens <- with(d, splinefun(x, cumsum(y) / sum(y), method="monoH.FC"))
  # should be something more like `integrate(ddens)`

  qdens <- with(d, splinefun(cumsum(y) / sum(y), x, method="monoH.FC"))
  # should be something like `inverse(pdens)`

  list(ddens = ddens, pdens = pdens, qdens = qdens)
}

#' Arbitrary Distribution
#'
#' Density, distribution function, quantile function, and random generation for
#' the `density()` object. Each function calls the appropriate function from the
#' output of `smooth_density()`.
#' @param sdens List outputted by `smooth_density()`.
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' @param p vector of probabilities (should be between 0 and 1).
#' @param n number of observations.
#'
#' @name dens
#' @seealso `stats::{dpqr}*()` distribution functions and logspline package.
#' @examples
#' set.seed(123)
#' data <- rnorm(10000)
#' dens <- density(data)
#'
#' sdens <- smooth_density(dens)
#'
#' ddens(sdens, 2)
#' pdens(sdens, 2)
#' qdens(sdens, 0.977)
#' rdens(sdens, 10L)
#'
NULL

#' @export
#' @rdname dens
ddens <- function(sdens, x) sdens$ddens(x)

#' @export
#' @rdname dens
pdens <- function(sdens, q) sdens$pdens(q)

#' @export
#' @rdname dens
qdens <- function(sdens, p) sdens$qdens(p)

#' @export
#' @rdname dens
rdens <- function(sdens, n) sdens$qdens(stats::runif(n))
