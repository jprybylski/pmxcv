testthat::test_that("numeric CV% is within allowed margin of expected CV%", {

  # test with pre-specified mean and sd
  test_mean = 5
  test_sd = sqrt(0.25)
  expected_cv = 100*test_sd/test_mean # 10
  allowed_margin = 1/100
  sample_size = 10^6
  test_x <- rnorm(sample_size,mean=test_mean,sd=test_sd)


  testthat::expect_lt(
    abs(numcv(test_x)/expected_cv - 1),
    allowed_margin
  )

  # Test with random mean and sd, with pre-specified cv%
  test_mean = sample(100,1)
  expected_cv = 30
  test_sd = expected_cv/100*test_mean
  test_x <- rnorm(sample_size,mean=test_mean,sd=test_sd)

  testthat::expect_lt(
    abs(numcv(test_x)/expected_cv - 1),
    allowed_margin
  )

  # Test with lognormal distribution
  test_mean = runif(1, 0.01, 100)
  expected_cv = 30
  test_sd = sqrt(log((expected_cv/100)^2 + 1))
  test_x <- rlnorm(sample_size,mean=log(test_mean),sd=test_sd)

  testthat::expect_lt(
    abs(numcv(test_x)/expected_cv - 1),
    allowed_margin
  )
})
