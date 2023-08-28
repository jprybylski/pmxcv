#' Integration-based CV%
#'
#' @param ... Arguments passed to moment()
#'
#' @return Percent CV
#' @export
#'
#' @examples
intcv <- function(...) {
  mom1 <- moment(n=1,...)
  mom2 <- moment(n=2,...)
  100*sqrt( mom2/mom1^2 - 1 )
}
