test_that("statistical moments are calculated correctly", {
  # normal distribution
  mean <- 1 # mean (moment1)
  var <- 0.5 # variance (moment2 - mean^2)
  # first moment
  testthat::expect_equal(moment(n=1,u=mean,v=var,pdist=c,qdist=c), mean)
  # second moment
  testthat::expect_equal(moment(n=2,u=mean,v=var,pdist=c,qdist=c), mean^2 + var)

  # lognormal distribution
  # lognormal moment generating function compared to integration method
  mean <- sample(10:1000,1) # natural units
  var <- runif(1, 0.5,2)
  mgf <- function(n) exp(n*log(mean) + (n^2)*var/2)
  # first moment
  testthat::expect_equal(moment(n=1,u=mean,v=var,pdist=exp,qdist=log), mgf(1))
  # second moment
  testthat::expect_equal(moment(n=2,u=mean,v=var,pdist=exp,qdist=log), mgf(2))

  # logit distribution
  # logit normal has no MGF, so solve numerically
  mean <- runif(1, min=0.1, max=0.9) # natural units
  var <- runif(1, min=0.3, 2)
  samp_n <- 10^6
  x_vals <- plogis(rnorm(samp_n, mean = qlogis(mean), sd = sqrt(var)))
  num_mo1 <- base::mean(x_vals)
  num_mo2 <- num_mo1^2 + stats::var(x_vals)
  test_mo1 <- moment(n=1,u=mean,v=var,pdist=plogis,qdist=qlogis)
  test_mo2 <- moment(n=2,u=mean,v=var,pdist=plogis,qdist=qlogis)
  rel_tol <- 1/100
  # first moment
  testthat::expect_lt(abs(num_mo1/test_mo1 - 1), rel_tol)
  # second moment
  testthat::expect_lt(abs(num_mo2/test_mo2 - 1), rel_tol)
})
