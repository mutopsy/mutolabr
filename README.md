# mutolabr: Convenient R Functions Frequently Used in mutolab

## Overview  

The `mutolabr` R package provides convenient functions frequently used in [mutolab](https://mutopsy.net/). This package includes functions to:  

* Conduct frequentist and Bayesian t-tests with effect size estimation (`t_test_all`).  
* Perform correlation tests or partial correlation tests using frequentist and Bayesian methods for all variables in a given dataset (`cor_test_all`, `pcor_test_all`).
* Perform contrast tests using frequentist method (`contrast_test`).  
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
* `pcor_test_all`: Perform Partial Correlation Tests with Frequentist Methods [[Document](https://mutopsy.github.io/mutolabr/reference/pcor_test_all.html)]
* `contrast_test`: Perform Contrast Tests with Frequentist Method [[Document](https://mutopsy.github.io/mutolabr/reference/contrast_test.html)]
* `trend_test`: Perform Trend Analysis using Contrast Test with Frequentist Method [[Document](https://mutopsy.github.io/mutolabr/reference/trend_test.html)]

### Compute Summary Statistics

* `summary_stat`: Compute Summary Statistics [[Document](https://mutopsy.github.io/mutolabr/reference/summary_stat.html)]
* `var_desc`: Compute Population Variance [[Document](https://mutopsy.github.io/mutolabr/reference/var_desc.html)]
* `sd_desc`: Compute Population Standard Deviation [[Document](https://mutopsy.github.io/mutolabr/reference/sd_desc.html)]
* `mode_stat`: Compute the Mode of a Distribution [[Document](https://mutopsy.github.io/mutolabr/reference/mode_stat.html)]
* `hdi`: Compute the Highest Density Interval (HDI) [[Document](https://mutopsy.github.io/mutolabr/reference/hdi.html)]
* `pdir`: Probability of Direction (pd) Calculation [[Document](https://mutopsy.github.io/mutolabr/reference/pdir.html)]

## Version History

### v1.3.0 (2025-04-30)   
- Implemented `pcor_test_all()`.
- Refined output formatting in `cor_test_all()` when using the Kendall method.

### v1.2.1 (2025-03-28)   
- Refined output formatting in `contrast_test()`.

### v1.2.0 (2025-03-27)   
- Implemented `trend_test()`.

### v1.1.0 (2025-03-26)   
- Implemented `contrast_test()`.

### v1.0.0 (2025-02-28)  
- Initial release of `mutolabr` package.  
- Implemented `t_test_all()`, `cor_test_all()`, `summary_stat()`, `var_desc()`, `sd_desc()`, `mode_stat()`, `hdi()`, and `pdir()`.

