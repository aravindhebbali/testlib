<!-- README.md is generated from README.Rmd. Please edit that file -->
testlib
=======

[![Travis-CI Build Status](https://travis-ci.org/rsquaredacademy/testlib.svg?branch=master)](https://travis-ci.org/rsquaredacademy/testlib) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/testlib?branch=master&svg=true)](https://ci.appveyor.com/project/rsquaredacademy/testlib) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/testlib)](http://cran.r-project.org/package=testlib) [![Coverage Status](https://img.shields.io/codecov/c/github/rsquaredacademy/testlib/master.svg)](https://codecov.io/github/rsquaredacademy/testlib?branch=master)

Overview
--------

testlib is a demo R package created to explore the different aspects of R package development. Hadley Wickham's book [R Packages](http://r-pkgs.had.co.nz/) was used as a guide for developing this package. The package contains a function for creating two way tables of categorical data and visualizing such tables using bar and mosaic plots. Learn more in `vignette("testlib")` or `?cross_table`.

Installation
------------

testlib is not currently available from CRAN, but you can install the development version from github with:

``` r
# install.packages("devtools")
devtools::install_github("rsquaredacademy/testlib")
```

Usage
-----

The main function in the pacakge is `cross_table()` which creates two way tables of categorical data.

``` r
library(testlib)
k <- testlib::cross_table(mtcars$cyl, mtcars$am)
k
```

    ##     Cell Contents
    ##  |---------------|
    ##  |     Frequency |
    ##  |       Percent |
    ##  |       Row Pct |
    ##  |       Col Pct |
    ##  |---------------|
    ## 
    ##  Total Observations:  32 
    ## 
    ## -------------------------------------------------------------
    ## |              |                     am                     |
    ## -------------------------------------------------------------
    ## |          cyl |            0 |            1 |    Row Total |
    ## -------------------------------------------------------------
    ## |            4 |            3 |            8 |           11 |
    ## |              |        0.094 |         0.25 |              |
    ## |              |         0.27 |         0.73 |         0.34 |
    ## |              |         0.16 |         0.62 |              |
    ## -------------------------------------------------------------
    ## |            6 |            4 |            3 |            7 |
    ## |              |        0.125 |        0.094 |              |
    ## |              |         0.57 |         0.43 |         0.22 |
    ## |              |         0.21 |         0.23 |              |
    ## -------------------------------------------------------------
    ## |            8 |           12 |            2 |           14 |
    ## |              |        0.375 |        0.062 |              |
    ## |              |         0.86 |         0.14 |         0.44 |
    ## |              |         0.63 |         0.15 |              |
    ## -------------------------------------------------------------
    ## | Column Total |           19 |           13 |           32 |
    ## |              |        0.594 |        0.406 |              |
    ## -------------------------------------------------------------
