test_that("nonmem box-cox transforms as expected from reference", {
  # Reference defining NONMEM-typic Box-Cox
  # Petersson et al. Pharm Res. 2009;26(9):2174-2185.
  omega_val <- runif(1,0.1,2)
  potential_lambdas <- c(runif(1,-2,-0.1), runif(1,0.1,2))
  lambda_test <- sample(potential_lambdas,1)
  pop_param <- runif(1, 0.1,10)
  samp_n <- 10^6
  eta_vals <- rnorm(samp_n,mean=0, sd=sqrt(omega_val))
  eta_exps <- exp(eta_vals)
  eta_tranforms <- ((eta_exps^lambda_test) - 1)/lambda_test
  indiv_params_para <- pop_param*exp(eta_vals) # The parameters if they were not transformed
  indiv_params <- pop_param*exp(eta_tranforms)
  # Because eta_transform is exponentiated, the variability is treated as lognormal
  # so the "transformed" parameter is actually
  indiv_transformed <- log(indiv_params)

  test_vals <- nonmemboxcox(indiv_params_para, lambda = lambda_test, theta=pop_param)

  testthat::expect_equal(median(test_vals), median(indiv_transformed))

})

test_that("nonmem box-cox is reversible", {
  potential_lambdas <- c(runif(1,-2,-0.1), runif(1,0.1,2))
  lambda_test <- sample(potential_lambdas,1)
  pop_param <- runif(1, 0.1,10)
  samp_n <- 10^6
  omega_val <- runif(1,0.1,2)
  eta_vals <- rnorm(samp_n,mean=0, sd=sqrt(omega_val))
  eta_exps <- exp(eta_vals)
  indiv_params_para <- pop_param*exp(eta_vals)

  test_x_bc <- nonmemboxcox(indiv_params_para, lambda = lambda_test, theta=pop_param)
  test_x <- nonmemboxcox(test_x_bc, lambda = lambda_test, theta=pop_param, inv = TRUE)

  testthat::expect_equal(median(test_x), median(indiv_params_para))
})

test_that("nonmem box-cox is log-transform when lamda=0", {

  test_val <- runif(1, 0.1, 100)
  test_transform <- nonmemboxcox(test_val, lambda=0, theta=1)

  testthat::expect_equal(test_transform, log(test_val))

  test_val2 <- log(test_val)
  test_transform2 <- nonmemboxcox(test_val2, lambda=0, theta=1, inv=TRUE)

  testthat::expect_equal(test_transform2, test_val)

})
