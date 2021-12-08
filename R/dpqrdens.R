#' Fit Density
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
#' fdens <- smooth_density(dens)
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
#' @param fd List outputted by `smooth_density()`.
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
#' fdens <- smooth_density(dens)
#'
#' ddens(fdens, 2)
#' pdens(fdens, 2)
#' qdens(fdens, 0.977)
#' rdens(fdens, 10L)
#'
NULL

#' @export
#' @rdname dens
ddens <- function(fd, x) fd$ddens(x)

#' @export
#' @rdname dens
pdens <- function(fd, q) fd$pdens(q)

#' @export
#' @rdname dens
qdens <- function(fd, p) fd$qdens(p)

#' @export
#' @rdname dens
rdens <- function(fd, n) fd$qdens(stats::runif(n))
