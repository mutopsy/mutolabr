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
* `withr`
* `ggplot2`

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
* `summary_generalTestBF`: Summarize Results from `generalTestBF` (beta version) [[Document](https://mutopsy.github.io/mutolabr/reference/summary_generalTestBF.html)]
* `anovakun_`: Perform ANOVA using the anovakun procedure (version 4.8.9). [[Document](https://mutopsy.github.io/mutolabr/reference/anovakun_.html)]
  * **Note:** This function is a direct copy of [`anovakun`](https://riseki.cloudfree.jp/?ANOVA%E5%90%9B), developed by Prof. Ryuta Iseki. Redistribution and minor modifications are permitted under the original author's policy, provided that such modifications are clearly stated.
* `anovakun_tidy`: A wrapper for anovakun with improved usability and formatting. [[Document](https://mutopsy.github.io/mutolabr/reference/anovakun_tidy.html)]
* `anova_bf`: Perform Bayesian ANOVA Using generalTestBF. [[Document](https://mutopsy.github.io/mutolabr/reference/anova_bf.html)]

### Compute Summary Statistics

* `summary_stat`: Compute Summary Statistics [[Document](https://mutopsy.github.io/mutolabr/reference/summary_stat.html)]
* `var_desc`: Compute Population Variance [[Document](https://mutopsy.github.io/mutolabr/reference/var_desc.html)]
* `sd_desc`: Compute Population Standard Deviation [[Document](https://mutopsy.github.io/mutolabr/reference/sd_desc.html)]
* `mode_stat`: Compute the Mode of a Distribution [[Document](https://mutopsy.github.io/mutolabr/reference/mode_stat.html)]
* `hdi`: Compute the Highest Density Interval (HDI) [[Document](https://mutopsy.github.io/mutolabr/reference/hdi.html)]
* `pdir`: Probability of Direction (pd) Calculation [[Document](https://mutopsy.github.io/mutolabr/reference/pdir.html)]

### Others
* `theme_gg0`: A Default ggplot2 Theme [[Document](https://mutopsy.github.io/mutolabr/reference/theme_gg0.html)]

## Version History

See the [Changelog on pkgdown site](https://mutopsy.github.io/mutolabr/news/) for the full changelog.

