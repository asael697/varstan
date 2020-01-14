#' Bvar: Quarterly Database for Macroeconomic Research
#'
#'  Bvar is a subsample differenced and at logarithm scale of the quarterly frequency companion
#'  to FRED-MD, a large macroeconomic database. It is designed to emulate the dataset used in
#'  "Disentangling the Channels of the 2007-2009 Recession" by Stock and Watson (2012), but also
#'  contains several additional series. The currently included dataset is from April 2019,
#'  contains observations from 1959-Q1 until 2018-Q4, and has been subset to series that either
#'  are in public domain, or we were given permission to use.
#'
#' @format A matrix with 240 observations of 3 variables.
#'
#' @source \pkg{BVAR}
#' https://research.stlouisfed.org/econ/mccracken/fred-databases/
#'
#' @references
#' McCracken, M. W., and Ng, S. (2016). FRED-MD: A Monthly Database for Macroeconomic Research.
#' Journal of Business & Economic Statistics, 34, 574-589.
#' https://doi.org/10.1080/07350015.2015.1086655
#'
#' Stock, J. H. and Watson, M. W. (2012). Disentangling the Channels of the 2007-2009 Recession.
#'  NBER Working Paper Series, 18094. https://doi.org/10.3386/w18094
#'
"Bvar"
