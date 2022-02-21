
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covidtest

<!-- badges: start -->

[![CI-CD](https://github.com/jesse-smith/covidtest/workflows/CI-CD/badge.svg)](https://github.com/jesse-smith/covidtest/actions)
[![Codecov test
coverage](https://codecov.io/gh/jesse-smith/covidtest/branch/master/graph/badge.svg)](https://codecov.io/gh/jesse-smith/covidtest?branch=master)
<!-- badges: end -->

{covidtest} is an R package and Shiny app designed to help organizations
evaluate their risk from COVID-19 and the potential benefits of regular
asymptomatic testing.

## Installation

You can install the development version of {covidtest} from Github with:

``` r
if (!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("jesse-smith/covidtest")
```

If you are using R on Windows, you will need to first install Rtools
[here](https://cran.r-project.org/bin/windows/Rtools/).

## Example

``` r
library(covidtest)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
```

To run the Shiny app, you can use `run_app()` or visit an [online
version](https://jesse-shiny.shinyapps.io/covidtest/).

To access the underlying model in **R**, you can use `ct_dist()` with
the desired parameters. Running with no inputs uses the defaults; the
output is a `data.table`:

``` r
# Create some data using default parameters
data <- ct_dist()

# Show data
data
#>                p   vac   inf  symp  test detect
#>  1: 7.083333e-04  TRUE  TRUE  TRUE  TRUE   TRUE
#>  2: 1.250000e-04  TRUE  TRUE  TRUE  TRUE  FALSE
#>  3: 0.000000e+00  TRUE  TRUE  TRUE FALSE   TRUE
#>  4: 0.000000e+00  TRUE  TRUE  TRUE FALSE  FALSE
#>  5: 3.035714e-04  TRUE  TRUE FALSE  TRUE   TRUE
#>  6: 5.357143e-05  TRUE  TRUE FALSE  TRUE  FALSE
#>  7: 0.000000e+00  TRUE  TRUE FALSE FALSE   TRUE
#>  8: 2.142857e-03  TRUE  TRUE FALSE FALSE  FALSE
#>  9: 0.000000e+00  TRUE FALSE  TRUE  TRUE   TRUE
#> 10: 0.000000e+00  TRUE FALSE  TRUE  TRUE  FALSE
#> 11: 0.000000e+00  TRUE FALSE  TRUE FALSE   TRUE
#> 12: 0.000000e+00  TRUE FALSE  TRUE FALSE  FALSE
#> 13: 0.000000e+00  TRUE FALSE FALSE  TRUE   TRUE
#> 14: 7.095238e-02  TRUE FALSE FALSE  TRUE  FALSE
#> 15: 0.000000e+00  TRUE FALSE FALSE FALSE   TRUE
#> 16: 4.257143e-01  TRUE FALSE FALSE FALSE  FALSE
#> 17: 1.416667e-03 FALSE  TRUE  TRUE  TRUE   TRUE
#> 18: 2.500000e-04 FALSE  TRUE  TRUE  TRUE  FALSE
#> 19: 0.000000e+00 FALSE  TRUE  TRUE FALSE   TRUE
#> 20: 0.000000e+00 FALSE  TRUE  TRUE FALSE  FALSE
#> 21: 6.071429e-04 FALSE  TRUE FALSE  TRUE   TRUE
#> 22: 1.071429e-04 FALSE  TRUE FALSE  TRUE  FALSE
#> 23: 0.000000e+00 FALSE  TRUE FALSE FALSE   TRUE
#> 24: 4.285714e-03 FALSE  TRUE FALSE FALSE  FALSE
#> 25: 0.000000e+00 FALSE FALSE  TRUE  TRUE   TRUE
#> 26: 0.000000e+00 FALSE FALSE  TRUE  TRUE  FALSE
#> 27: 0.000000e+00 FALSE FALSE  TRUE FALSE   TRUE
#> 28: 0.000000e+00 FALSE FALSE  TRUE FALSE  FALSE
#> 29: 0.000000e+00 FALSE FALSE FALSE  TRUE   TRUE
#> 30: 7.047619e-02 FALSE FALSE FALSE  TRUE  FALSE
#> 31: 0.000000e+00 FALSE FALSE FALSE FALSE   TRUE
#> 32: 4.228571e-01 FALSE FALSE FALSE FALSE  FALSE
#>                p   vac   inf  symp  test detect
```

You can access the risk-based metrics used in the Shiny app using
corresponding functions in the R package. Undetected cases are
calculated using `ct_undetected()`, relative risk reduction is
calculated using `ct_rr()`, and cost effectiveness per test is
calculated using `ct_cost_eff()`.

``` r
# Cost effectiveness
ct_undetected(data)
#> [1] 0.006964286

# Relative risk reduction
ct_rr(data)
#> [1] 0.1156463

# Cost effectiveness
ct_cost_eff(data)
#> [1] 0.02093596
```

Additionally, test performance metrics are included in the R package to
facilitate analysis not performed in the Shiny app. These include:

-   `ct_pos()`: the proportion of positive tests (out of the
    organization)
-   `ct_neg()`: the proportion of negative tests (out of the
    organization)
-   `ct_true_pos()`: the proportion of true positive tests (out of org)
-   `ct_true_neg()`: the proportion of true negative tests (out of org)
-   `ct_false_pos()`: the proportion of false positive tests (out of
    org)
-   `ct_false_neg()`: the proportion of false negative tests (out of
    org)
-   `ct_ppv()`: the positive predictive value of a test
-   `ct_npv()`: the negative predictive value of a test
-   `ct_fdr()`: the false discovery rate of a test
-   `ct_for()`: the false omission rate of a test
-   `ct_sens()`: the sensitivity (true positive rate, recall) of a test
-   `ct_spec()`: the specificity (true negative rate) of a test
-   `ct_fpr()`: the false positive rate of a test
-   `ct_fnr()`: the false negative rate of a test

``` r
# Positive tests
ct_pos(data)
#> [1] 0.003035714

# Negative tests
ct_neg(data)
#> [1] 0.1419643

# True positives
ct_true_pos(data)
#> [1] 0.003035714

# True negatives
ct_true_neg(data)
#> [1] 0.1414286

# False positives
ct_false_pos(data)
#> [1] 0

# False negatives
ct_false_neg(data)
#> [1] 0.0005357143

# Positive predictive value (precision)
ct_ppv(data)
#> [1] 1

# Negative predictive value
ct_npv(data)
#> [1] 0.9962264

# False discovery rate
ct_fdr(data)
#> [1] 0

# False omission rate
ct_for(data)
#> [1] 0.003773585

# True positive rate (sensitivity/recall)
ct_sens(data)
#> [1] 0.85

# True negative rate (specificity)
ct_spec(data)
#> [1] 1

# False positive rate
ct_fpr(data)
#> [1] 0

# False negative rate
ct_fnr(data)
#> [1] 0.15
```

## Code of Conduct

Please note that the {covidtest} project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
