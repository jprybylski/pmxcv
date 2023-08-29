
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmxcv

<!-- badges: start -->
<!-- badges: end -->

This package is intended to provide easy access to methods of reporting
CV% in non-lognormal, non-normal distributions. Often in pharmacometric
literature, CV% is only reported for lognormal variability (or treated
as lognormal), favoring reporting of the harder-to-interpret variance
parameter; this package attempts to provide an alternative approach.

## Installation

You can install the development version of pmxcv like so:

``` r
devtools::install_github("pfizer-rd/pmxcv")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pmxcv)

## Parameters from NONMEM (etc)
theta_bio <- 0.689
omega_bio <- 1.2

# Representation of bioavailability calculation in NONMEM syntax:L
# LOGITF1 = LOG( THETA(BIO) ) - LOG( 1 - THETA(BIO) )
# F1 = 1/( 1 + 1/EXP( LOGITF1 + ETA(BIO) )

## Numeric parameter variability
eta_sample <- rnorm(10^9, sd=sqrt(omega_bio))
logit_theta <- log(theta_bio) - log( 1 - theta_bio )
indiv_bios <- 1 / ( 1 + 1/exp( logit_theta + eta_sample ) )
expected_cv <- 100*sd(indiv_bios)/mean(indiv_bios)
expected_cv
#> [1] 31.3978

## Lognormal reported CV% (erroneous)
bio_cv_lnorm <- 100*sqrt(exp(omega_bio) - 1)
bio_cv_lnorm
#> [1] 152.3193

## Logitnormal reported CV%
bio_cv_lgtnorm <- dist.intcv("logit", u=theta_bio, v=omega_bio)
bio_cv_lgtnorm
#> [1] 31.39758
```
