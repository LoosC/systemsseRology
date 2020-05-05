
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

systemsseRology is a collection of functions used to analyse
multivariate systems serology data. You can learn more about the
workflow in `vignette("exampleAnalysis")`.

## Installation

This requires the package devtools.

``` r
install.packages("devtools")
install_github("LoosC/systemsseRology")
library(systemsseRology)
```
and the package 'ropls'

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("ropls")
```
