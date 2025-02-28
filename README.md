# mutolabr

## Overview  

The `mutolabr` R package provides convenient functions frequently used in mutolab. This package includes functions to:  

* Conduct frequentist and Bayesian t-tests with effect size estimation (`t_test_all`).  
* Perform correlation tests using frequentist and Bayesian methods for all variables in a given dataset (`cor_test_all`).  
* Compute summary statistics for a single variable, including the highest density interval (`hdi`), mode (`mode_stat`), population variance (`var_desc`), and population standard deviation (`sd_desc`).  
* Calculate the probability of direction (pd) from MCMC samples of a posterior distribution.  
* Retrieve a list of currently loaded R packages along with their versions (`loaded_packages_version`).  

## Installation  

You can install `mutolabr` from GitHub using `devtools`:  

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

# Install mutolabr
devtools::install_github("mutopsy/mutolabr")
```

## Dependencies  

This package requires:  
* R (>= 4.1.0)  
* `dplyr`  
* `tidyr`  
* `tibble`  
* `BayesFactor`  
* `effectsize`  

## Usage

After installation, you can load this package using the `library` function.

```r
library(mutolabr)
```

## List of Functions

For details on each function, refer to [the function references](https://mutopsy.github.io/mutolabr/reference/).

### Statistical Tests

* `t_test_all`: Perform Frequentist and Bayesian t-tests with Effect Size Estimation [[Document](https://mutopsy.github.io/mutolabr/reference/t_test_all.html)]
* `cor_test_all`: Perform Correlation Tests with Frequentist and Bayesian Methods [[Document](https://mutopsy.github.io/mutolabr/reference/cor_test_all.html)]

### Compute Summary Statistics

* `summary_stat`: Compute Summary Statistics [[Document](https://mutopsy.github.io/mutolabr/reference/summary_stat.html)]
* `var_desc`: Compute Population Variance [[Document](https://mutopsy.github.io/mutolabr/reference/var_desc.html)]
* `sd_desc`: Compute Population Standard Deviation [[Document](https://mutopsy.github.io/mutolabr/reference/sd_desc.html)]
* `mode_stat`: Compute the Mode of a Distribution [[Document](https://mutopsy.github.io/mutolabr/reference/mode_stat.html)]
* `hdi`: Compute the Highest Density Interval (HDI) [[Document](https://mutopsy.github.io/mutolabr/reference/hdi.html)]
* `pdir`: Probability of Direction (pd) Calculation [[Document](https://mutopsy.github.io/mutolabr/reference/pdir.html)]
