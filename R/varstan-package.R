#' The 'varstan' package.
#'
#' @description The goal of this package is analyze time series with structured models
#' such as seasonal ARIMA, GARCh and its variants, VARMA, Bekk, Stochastic Volatility Models,
#' Hidden Markov Models and univariate state space models. So far a Sarima,
#' GARCH, dynamic regression, and varma models are available for the users.
#'
#' @docType package
#' @name varstan-package
#'
#' @useDynLib varstan, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstan loo
#'
#' @references
#' Stan Development Team (2019). RStan: the R interface to Stan. R package version 2.19.2. https://mc-stan.org
#'
#' Hyndman and Khandakar, Automatic Time Series Forecasting: The forecast package for R.
#'
#' Hyndman Athanasopoulos, Forecasting: Principles and Practice.
#'
#' Engle, Robert F. (1982). ?Autoregressive Conditional Heteroscedasticity with Estimates of the Variance of United Kingdom Inflation.
#'
NULL
