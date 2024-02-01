#' Numeric CV% of a sample
#'
#' @param x numeric vector
#' @param ... other arguments for sd() and mean()
#'
#' @return Percent cv
#' @export
#'
#' @importFrom stats sd
#'
#' @examples
#' test_x <- rnorm(1000, mean=50, sd=5)
#' cv <- numcv(test_x)
#' cv # expect ~ 10(%)
numcv <- function(x,...) {
  100*sd(x,...)/mean(x,...)
}
