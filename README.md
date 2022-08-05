
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sCRM <img src='inst/app/www/hexSticker_scrm.png' align="right" height="139" />

A web-based application that is essentially a wrapper of the
[`{stochLAB}`](https://www.github.com/HiDef-Aerial-Surveying/stochLAB)
package, offering a user-friendly interactive platform to run Collision
Risk Models (CRMs) for seabird species under multiple wind farm
scenarios.

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

sCRM’s current release is a Beta version and will be undergoing testing.
It is intended to eventually replace the
[stochCRM](https://github.com/dmpstats/stochCRM) tool.

## How to Use

Users have the option to use an online version of the app or install and
run it locally.

### On a local machine

Assuming that a recent version of [R](https://cran.r-project.org/)
(\>4.1) has been installed, run the following commands to install
`{sCRM}`:

``` r
# install.packages("remotes")
devtools::install_github("dmpstats/sCRM")
```

Then, to launch the app:

``` r
library(sCRM)
sCRM::run_app()
```

### Online

sCRM can be accessed online on <https://dmpstats.shinyapps.io/sCRM/>

## To Do List

-   Introductory tour of the tool for first time users
-   Feature to upload inputs from external spreadsheets
-   User Manual update
-   State bookmarking (?)
