#' Built-in moment functions
#'
#' @param dist Selection of built-in distributions.
#' @param ... passed to moment()
#' @param exact If there is an exact moment generating function, use that. Default TRUE only for log
#' @param lambda shape parameter for nonmemboxcox()
#'
#' @return moment
#' @export
#'
#' @importFrom stats plogis qlogis
#'
#' @examples
dist.moment <- function(dist="log",...,exact=ifelse(dist=="log",TRUE,FALSE),lambda=NULL) {
  out <- NULL
  if (dist=="log") {
    list2env(list(...), environment())
    if (exact) out <- exp(n*log(u) + (n^2)*v/2) # u expected for exact
    else out <- moment(...,pdist = exp,qdist= log)
  } else if (dist == "logexp") {
    out <- moment(...,pdist = function(x) exp(x) - 1,qdist= function(x) log(x+1))
  } else if (dist == "logit") {
    out <- moment(...,pdist = plogis,qdist= qlogis)
  } else if (dist == "arcsin") {
    out <- moment(...,pdist = function(x) sin(x)^2,qdist= function(x) asin(sqrt(x)))
  } else if (dist == "nmboxcox") {
    list2env(list(...), environment())
    qbxcxt <- function(x) nonmemboxcox(x,lambda=lambda, theta = u)
    pbxcxt <- function(x) nonmemboxcox(x,lambda=lambda, theta = u,inv=TRUE)
    out <- moment(...,pdist = pbxcxt,qdist= qbxcxt)
  }
  if (is.null(out)) stop(sprintf("Distribution %s not built into package.", dist))
  out
}
