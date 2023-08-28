#' Numeric CV% of a sample
#'
#' @param x numeric vector
#' @param ... other arguments for sd() and mean()
#'
#' @return Percent cv
#' @export
#'
#' @examples
numcv <- function(x,...) {
  100*sd(x,...)/mean(x,...)
}
