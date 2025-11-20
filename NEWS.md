# mutolabr 1.9.0 (2025-11-20)
* Added new functions for calculating visual angles.
* Specifically, implemented `visualangle()`, `inch_to_cm()`, `radian_to_degree()`, and `degree_to_radian()`.

# mutolabr 1.8.4 (2025-09-23)
* Minor fix for print methods for `t_test_all_tidy` and `t_test_all_tidy_grouped`.
* Correct typos of documents.

# mutolabr 1.8.2 (2025-09-22)
* Support one-sample design for `t_test_all_tidy` and `t_test_all_tidy_grouped`.
* Make testthat for `t_test_all_tidy` and `t_test_all_tidy_grouped`.

# mutolabr 1.8.1 (2025-09-21)
* Minor bug fixed.

# mutolabr 1.8.0 (2025-09-20)
* Implemented `t_test_all_tidy_grouped()` and `p_to_holmalpha()`.

# mutolabr 1.7.0 (2025-09-10)
* Implemented `t_test_all_tidy()`.
* Minor bug fixed.

# mutolabr 1.6.0 (2025-09-09)
* Implemented `anova_bf()`.

# mutolabr 1.5.0 (2025-07-24)
* Implemented `anovakun()`, a direct copy of `anovakun` version 4.8.9, originally developed by Prof. Ryuta Iseki (Taisho University, Japan). Redistribution and minor modifications of the original function are permitted under the terms specified by the original author, provided that such modifications are clearly stated. This implementation includes no modifications to the computational logic and complies with that policy. For details, see [the function documentation](https://mutopsy.github.io/mutolabr/reference/anovakun.html); the original documentation by Prof. Iseki is available at: https://riseki.cloudfree.jp/?ANOVA%E5%90%9B

* Implemented `anovakun_tidy()`, a wrapper around `anovakun()` that improves usability. This function automatically determines the number of factor levels from the dataset, formats the output as a tidy data frame, and adds commonly used effect size measures and rounding options to facilitate reporting and interpretation.

# mutolabr 1.4.0 (2025-06-22)   
* Implemented `rmANOVA_bf()`, `summary_generalTestBF()`, and `theme_gg0()`.

# mutolabr 1.3.0 (2025-04-30)   
* Implemented `pcor_test_all()`.
* Refined output formatting in `cor_test_all()` when using the Kendall method.

# mutolabr 1.2.1 (2025-03-28)   
* Refined output formatting in `contrast_test()`.

# mutolabr 1.2.0 (2025-03-27)   
* Implemented `trend_test()`.

# mutolabr 1.1.0 (2025-03-26)   
* Implemented `contrast_test()`.

# mutolabr 1.0.0 (2025-03-26)   
* Initial release of `mutolabr` package.
* Implemented `t_test_all()`, `cor_test_all()`, `summary_stat()`, `var_desc()`, `sd_desc()`, `mode_stat()`, `hdi()`, and `pdir()`.
