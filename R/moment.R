#' Moment function
#'
#' @param ... all arguments passed to moment_f()
#'
#' @return moment
#' @export
#'
#' @examples
#' moment(n = 3, u = 1, v = 0.2, pdist = exp, qdist = log)
moment <- function(...) {
  int_f <- function(x) moment_f(x,...)
  stats::integrate(int_f, -Inf, Inf, abs.tol = 0)$value
}
