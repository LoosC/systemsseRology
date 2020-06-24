
## Overview

systemsseRology is a collection of functions used to analyse
multivariate systems serology data. You can learn more about the
workflow in `vignette("example_analysis")`.

## Installation

This requires the package devtools.

``` r
install.packages("devtools")
install_github("LoosC/systemsseRology", ref = "reboot")
library(systemsseRology)
```
and the package 'ropls'

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("ropls")
```
