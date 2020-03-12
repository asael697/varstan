**VARSTAN NEWS**
============

**varstan 1.0.0.000 Date: 14/12/2019**
----------------------------------

### Features:

-   The object DWR is implemented for Dynamic Harmonic Regression

-   The object Bekk is implemented for Bekk models, and not just be
    accesed in varma class

-   The object Sarima is implemented for seasonal arima with regression

-   auto.sarima function for automatic fitting a bayesian Sarima model

-   Implementation of brige\_sampler, bayes\_factor, loo, waic, aic,
    bic, AICc methods for varstan objects

-   gps, sunspots, ipc, Bvar and birth data is added to the package

### Improvements:

-   Better presentation of varstan print methods

-   Correction of all the documentation and help

-   version, parameters and distributions, functions for facilities of
    the package usage

### Changes:

-   The arima class is replaced by Sarima

### Fixes:

-   No current fixes

**varstan 0.5.1.000 Date: 23/12/2019**
----------------------------------

### Features:

-   The method posterior\_predict and predictive\_error are implemented
    for the varstan class

-   The objets VarBekk and Varbekkm are replaced for the object varma

### Improvements:

-   Fixing the methods and overloaded functions for print, and summary

### Changes:

-   point\_estimate name change to posterior\_estimate

-   fit\_values change to posterior\_fit

-   get\_residuals change to posterior\_residuals

### Fixes:

-   Correction of the point\_estimate in mgarch parameter.

**varstan 0.5.0.000 Date: 11/11/2019**
----------------------------------

### Features:

-   The objects arima and garch model implemented

-   The objets VarBekk and Varbekkm are replaced for the object varma

### Improvements:

-   A general classs for ausing varma, varmabekk and varmabekkm models

### Changes:

-   The clases varbekk and varbekkm are replace for the general class
    varma, with a mbekk extension

### Fixes:

-   The function extract\_stan is fixed

-   The functions point estimate and summary are re program.

**varstan 0.0.1.000 Date: 26/09/2019**
----------------------------------

### Features:

-   The objets VarBekk and Varbekk model implemented for define the
    current model for the time series

-   The object varstan object implemented for estimating a one of the
    current available models

-   Precomplied stan code and ready for use

-   summary and point\_estimate functions overloaded for varstan
    objects.

### Improvements:

-   No current improvements

### Changes:

-   No current changes

### Fixes:

-   No current fixes