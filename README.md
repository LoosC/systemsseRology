# systemsseRology

### About
systemsseRology is a collection of functions used to analyse multivariate systems serology data. You can learn more about the workflow by looking at `vignette("example_analysis")`.

### Installation
systemsseRology requires the  ropls package:
``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("ropls")
```
It can then be installed via devtools:
``` r
install.packages("devtools")
install_github("LoosC/systemsseRology", ref = "reboot")
library(systemsseRology)
```

### Selected Papers
systemsseRology has been employed in a number of papers, including:
* Bartsch, Y. C. et. al. (2021). [Viral Rebound Kinetics Correlate with Distinct HIV Antibody Features. ](https://mbio.asm.org/content/12/2/e00170-21.abstract) Mbio, 12(2).

* Herman, J. D. et. al. (2021). [Functional Antibodies in COVID-19 Convalescent Plasma.]( https://www.medrxiv.org/content/10.1101/2021.03.08.21253157v1) medRxiv.

* Bartsch, Y. C. et. al. (2021). [Humoral signatures of protective and pathological SARS-CoV-2 infection in children.](https://www.nature.com/articles/s41591-021-01263-3) Nature medicine, 1-9.

* Gorman, M. J. et. al. (2021). [Collaboration between the Fab and Fc contribute to maximal protection against SARS-CoV-2 in nonhuman primates following NVX-CoV2373 subunit vaccine with Matrix-M™ vaccination.](https://www.biorxiv.org/content/10.1101/2021.02.05.429759v1.abstract) bioRxiv.

* Krystle, K. Q. et. al. (2021). [Comorbid illnesses are associated with altered adaptive immune responses to SARS-CoV-2.](https://insight.jci.org/articles/view/146242) JCI insight.

