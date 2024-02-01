test_that("built-in moments/cv%s are comparable to numeric moments/cv%s", {
  set.seed(652)
  samp_n <- 10^6
  rand_var <- runif(1, 0.3, 3)
  norm_vals <- rnorm(samp_n, sd=sqrt(rand_var))
  tolerance <- 5/100 # high tolerance, given comparison is to numeric

  # log
  lnorm_u <- exp(runif(1, -3, 3))
  lnorm_vals <- exp( log(lnorm_u) + norm_vals)


  lnorm_num_m1 <- mean(lnorm_vals)
  lnorm_bin_m1 <- dist.moment("log", u=lnorm_u, v=rand_var, n=1)
  expect_lt(abs(lnorm_bin_m1/lnorm_num_m1 - 1), tolerance)
  lnorm_num_m2 <- lnorm_num_m1^2 + var(lnorm_vals)
  lnorm_bin_m2 <- dist.moment("log", u=lnorm_u, v=rand_var, n=2)
  expect_lt(abs(lnorm_num_m2/lnorm_bin_m2 - 1), tolerance)
  lnorm_bin_m1_2 <- dist.moment("log", u=lnorm_u, v=rand_var, n=1, exact = FALSE)
  expect_lt(abs(lnorm_bin_m1_2/lnorm_num_m1 - 1), tolerance)
  lnorm_bin_m2_2 <- dist.moment("log", u=lnorm_u, v=rand_var, n=2, exact = FALSE)
  expect_lt(abs(lnorm_bin_m2_2/lnorm_bin_m2 - 1), tolerance)

  lnorm_numcv <- numcv(lnorm_vals)
  lnorm_intcv <- dist.intcv("log", u=lnorm_u, v=rand_var)
  expect_lt(abs(lnorm_intcv/lnorm_numcv - 1), tolerance)
  lnorm_intcv2 <- dist.intcv("log", u=lnorm_u, v=rand_var, exact=FALSE)
  expect_lt(abs(lnorm_intcv2/lnorm_numcv - 1), tolerance)
  lnorm_intcv3 <- dist.intcv("log", v=rand_var, exact=FALSE)
  expect_lt(abs(lnorm_intcv3/lnorm_numcv - 1), tolerance)

  #logexp
  logexp_u <- runif(1, -0.5, 0.1)
  logexp_vals <- exp( log(logexp_u + 1) + norm_vals) - 1


  logexp_num_m1 <- mean(logexp_vals)
  logexp_bin_m1 <- dist.moment("logexp", u=logexp_u, v=rand_var, n=1)
  expect_lt(abs(logexp_bin_m1/logexp_num_m1 - 1), tolerance)
  logexp_num_m2 <- logexp_num_m1^2 + var(logexp_vals)
  logexp_bin_m2 <- dist.moment("logexp", u=logexp_u, v=rand_var, n=2)
  expect_lt(abs(logexp_num_m2/logexp_bin_m2 - 1), tolerance)

  logexp_numcv <- abs(numcv(logexp_vals))
  logexp_intcv <- dist.intcv("logexp", u=logexp_u, v=rand_var)
  expect_lt(abs(logexp_intcv/logexp_numcv - 1), tolerance)

  #logit
  logit_u <- runif(1, 0.001, 0.999)
  logit_vals <- 1/(1 + 1/exp( log(logit_u) - log(1-logit_u) + norm_vals))


  logit_num_m1 <- mean(logit_vals)
  logit_bin_m1 <- dist.moment("logit", u=logit_u, v=rand_var, n=1)
  expect_lt(abs(logit_bin_m1/logit_num_m1 - 1), tolerance)
  logit_num_m2 <- logit_num_m1^2 + var(logit_vals)
  logit_bin_m2 <- dist.moment("logit", u=logit_u, v=rand_var, n=2)
  expect_lt(abs(logit_num_m2/logit_bin_m2 - 1), tolerance)

  logit_numcv <- numcv(logit_vals)
  logit_intcv <- dist.intcv("logit", u=logit_u, v=rand_var)
  expect_lt(abs(logit_intcv/logit_numcv - 1), tolerance)

  #arcsin
  arcsin_u <- runif(1, 0, 1)
  arcsin_vals <- sin(asin(sqrt(arcsin_u)) + norm_vals)^2


  arcsin_num_m1 <- mean(arcsin_vals)
  arcsin_bin_m1 <- dist.moment("arcsin", u=arcsin_u, v=rand_var, n=1)
  expect_lt(abs(arcsin_bin_m1/arcsin_num_m1 - 1), tolerance)
  arcsin_num_m2 <- arcsin_num_m1^2 + var(arcsin_vals)
  arcsin_bin_m2 <- dist.moment("arcsin", u=arcsin_u, v=rand_var, n=2)
  expect_lt(abs(arcsin_num_m2/arcsin_bin_m2 - 1), tolerance)

  arcsin_numcv <- numcv(arcsin_vals)
  arcsin_intcv <- dist.intcv("arcsin", u=arcsin_u, v=rand_var)
  expect_lt(abs(arcsin_intcv/arcsin_numcv - 1), tolerance)

  #nmboxcox (numeric is too unstable for this test)
  if (FALSE) {
    nmboxcox_u <- runif(1, 0.1, 10)
    potential_lambdas <- c(runif(1,-2,-0.5), runif(1,0.5,2))
    lambda_test <- sample(potential_lambdas,1)
    eta_exps <- exp(norm_vals)
    eta_tranforms <- ((eta_exps^lambda_test) - 1)/lambda_test
    nmboxcox_vals <- nmboxcox_u*exp(eta_tranforms)
    nmboxcox_vals <- nmboxcox_vals[is.finite(nmboxcox_vals) & abs(nmboxcox_vals)<10^6]


    nmboxcox_num_m1 <- mean(nmboxcox_vals)
    nmboxcox_bin_m1 <- dist.moment("nmboxcox", u=nmboxcox_u, v=rand_var, lambda=lambda_test, n=1)
    expect_lt(abs(nmboxcox_bin_m1/nmboxcox_num_m1 - 1), tolerance)
    nmboxcox_num_m2 <- nmboxcox_num_m1^2 + var(nmboxcox_vals)
    nmboxcox_bin_m2 <- dist.moment("nmboxcox", u=nmboxcox_u, v=rand_var, lambda=lambda_test, n=2)
    expect_lt(abs(nmboxcox_num_m2/nmboxcox_bin_m2 - 1), tolerance)

    nmboxcox_numcv <- numcv(nmboxcox_vals)
    nmboxcox_intcv <- dist.intcv("nmboxcox", u=nmboxcox_u, v=rand_var, lambda=lambda_test)
    expect_lt(abs(nmboxcox_intcv/nmboxcox_numcv - 1), tolerance)
  }
  nmboxcox_u <- runif(1, 0.1, 10)
  potential_lambdas <- c(runif(1,-2,-0.5), runif(1,0.5,2))
  lambda_test <- sample(potential_lambdas,1)
  nmboxcox_bin_m1 <- dist.moment("nmboxcox", u=nmboxcox_u, v=rand_var, lambda=lambda_test, n=1)
  nmboxcox_bin_m2 <- dist.moment("nmboxcox", u=nmboxcox_u, v=rand_var, lambda=lambda_test, n=2)

  nmboxcox_momentcv <- 100*sqrt( nmboxcox_bin_m2/nmboxcox_bin_m1^2 - 1 )
  nmboxcox_intcv <- dist.intcv("nmboxcox", u=nmboxcox_u, v=rand_var, lambda=lambda_test)
  expect_lt(abs(nmboxcox_intcv/nmboxcox_momentcv - 1), tolerance)

  ## Other functionality

  # return function
  fun.logitcv <- dist.intcv("logit", fun=TRUE)
  test_logitcv <-  fun.logitcv(u=logit_u, v=rand_var)
  expect_equal(test_logitcv,logit_intcv)
  fun.logcv <- dist.intcv("log", fun=TRUE)
  test_logcv <-  fun.logcv(v=rand_var)
  lnorm_intcv <- dist.intcv("log", v=rand_var)
  expect_equal(test_logcv,lnorm_intcv)

  # missing builtin errors
  expect_error(dist.moment("binomial", u=1, v=rand_var, n=1))
  expect_error(dist.intcv("binomial", u=1, v=rand_var))

})
