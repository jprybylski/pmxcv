test_that("integral cv is consistent with numeric or exact CV", {

  # Normal distribution
  mean <- runif(1, 0.5, 10) # mean
  var <- runif(1, 0.1, 5) # variance
  exact_cv <- 100*sqrt(var)/mean
  test_cv <- intcv(u=mean,v=var,pdist=c,qdist=c)
  rel_tol <- 1/100
  testthat::expect_lt(abs(test_cv/exact_cv - 1), rel_tol)

  # Lognormal distribution
  mean <- runif(1,0.1,10) # natural units
  var <- runif(1, 0.5,2)
  exact_cv <- 100*sqrt(exp(var) - 1) # independent of mean
  test_cv <- intcv(u=mean,v=var,pdist = exp,qdist= log)
  rel_tol <- 1/100
  testthat::expect_lt(abs(test_cv/exact_cv - 1), rel_tol)


  # Logit-normal distribution
  mean <- runif(1, min=0.1, max=0.9) # natural units
  var <- runif(1, min=0.3, 2)
  samp_n <- 10^6
  x_vals <- plogis(rnorm(samp_n, mean = qlogis(mean), sd = sqrt(var)))
  num_cv <- numcv(x_vals)
  test_cv <- intcv(u=mean,v=var,pdist = plogis,qdist= qlogis)
  rel_tol <- 1/100
  testthat::expect_lt(abs(test_cv/num_cv - 1), rel_tol)

})
