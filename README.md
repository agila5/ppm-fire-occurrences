

## README

This repository is associated with the paper *Non-separable
spatio-temporal Poisson point process models for fire occurrences* by
Nicoletta D’Angelo, Alessandro Albano, Andrea Gilardi, and Giada
Adelfio. It contains the code and the data necessary to reproduce the
analyses presented in the manuscript

We used the `R` software and the following contributed packages:

    ─ Session info ───────────────────────────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.3.1 (2023-06-16 ucrt)
     os       Windows 11 x64 (build 22631)
     system   x86_64, mingw32
     ui       RTerm
     language (EN)
     collate  English_United Kingdom.utf8
     ctype    English_United Kingdom.utf8
     tz       Europe/Rome
     date     2024-10-20
     pandoc   3.2 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

    ─ Packages ───────────────────────────────────────────────────────────────────────────────────────
     package          * version    date (UTC) lib source
     abind            * 1.4-8      2024-09-12 [1] CRAN (R 4.3.1)
     colorspace       * 2.1-1      2024-07-26 [1] CRAN (R 4.3.1)
     cols4all         * 0.7-1      2024-08-02 [1] Github (mtennekes/cols4all@48964df)
     conflicted       * 1.2.0      2023-02-01 [1] CRAN (R 4.3.1)
     dplyr            * 1.1.4      2023-11-17 [1] CRAN (R 4.3.1)
     forcats          * 1.0.0      2023-01-29 [1] CRAN (R 4.3.1)
     gdalUtilities    * 1.2.5      2023-08-10 [1] CRAN (R 4.3.1)
     ggplot2          * 3.5.1.9000 2024-09-10 [1] Github (tidyverse/ggplot2@2bd8cd5)
     here             * 1.0.1      2020-12-13 [1] CRAN (R 4.3.1)
     knitr            * 1.48       2024-07-07 [1] CRAN (R 4.3.1)
     lubridate        * 1.9.3      2023-09-27 [1] CRAN (R 4.3.1)
     mgcv             * 1.9-1      2023-12-21 [1] CRAN (R 4.3.1)
     nlme             * 3.1-162    2023-01-31 [2] CRAN (R 4.3.1)
     piggyback        * 0.1.5      2023-07-10 [1] CRAN (R 4.3.1)
     plot3D           * 1.4        2021-05-22 [1] CRAN (R 4.3.1)
     purrr            * 1.0.2      2023-08-10 [1] CRAN (R 4.3.1)
     qs               * 0.25.5     2023-02-22 [1] CRAN (R 4.3.1)
     quarto           * 1.4.4      2024-07-20 [1] CRAN (R 4.3.1)
     readr            * 2.1.4      2023-02-10 [1] CRAN (R 4.3.1)
     rmarkdown        * 2.28       2024-08-17 [1] CRAN (R 4.3.1)
     rpanel           * 1.1-5.2    2023-02-07 [1] CRAN (R 4.3.1)
     rpart            * 4.1.19     2022-10-21 [2] CRAN (R 4.3.1)
     scales           * 1.3.0      2023-11-28 [1] CRAN (R 4.3.1)
     sessioninfo      * 1.2.2      2021-12-06 [1] CRAN (R 4.3.1)
     sf               * 1.0-18     2024-10-04 [1] Github (r-spatial/sf@6f247a5)
     sp               * 2.1-4      2024-04-30 [1] CRAN (R 4.3.1)
     sparr            * 2.3-10     2023-03-08 [1] CRAN (R 4.3.1)
     spatstat         * 3.2-1      2024-09-22 [1] CRAN (R 4.3.1)
     spatstat.data    * 3.1-2      2024-06-21 [1] CRAN (R 4.3.1)
     spatstat.explore * 3.3-2      2024-08-21 [1] CRAN (R 4.3.1)
     spatstat.geom    * 3.3-3      2024-09-18 [1] CRAN (R 4.3.1)
     spatstat.linnet  * 3.2-2      2024-09-20 [1] CRAN (R 4.3.1)
     spatstat.model   * 3.3-2      2024-09-19 [1] CRAN (R 4.3.1)
     spatstat.random  * 3.3-2      2024-09-18 [1] CRAN (R 4.3.1)
     spatstat.univar  * 3.0-1      2024-09-05 [1] CRAN (R 4.3.1)
     splancs          * 2.01-44    2023-08-21 [1] CRAN (R 4.3.1)
     stars            * 0.6-7      2024-10-05 [1] Github (r-spatial/stars@ec1f849)
     stopp            * 0.2.4      2024-06-04 [1] CRAN (R 4.3.1)
     stpp             * 2.0-8      2024-06-28 [1] CRAN (R 4.3.1)
     stringr          * 1.5.1      2023-11-14 [1] CRAN (R 4.3.1)
     tarchetypes      * 0.7.12     2024-02-06 [1] CRAN (R 4.3.1)
     targets          * 1.4.1      2024-01-09 [1] CRAN (R 4.3.1)
     tibble           * 3.2.1      2023-03-20 [1] CRAN (R 4.3.1)
     tidyr            * 1.3.0      2023-01-24 [1] CRAN (R 4.3.1)
     tidyverse        * 2.0.0      2023-02-22 [1] CRAN (R 4.3.1)

     [1] C:/Users/user/AppData/Local/R/win-library/4.3
     [2] C:/Program Files/R/R-4.3.1/library

    ──────────────────────────────────────────────────────────────────────────────────────────────────

**Please check that all necessary packages (especially `targets`,
`tarchetypes`, and `piggyback`) are installed before going to the next
section.**

### Instructions

The following steps are necessary to locally reproduce the results
included in the manuscript. Please note that:

A. We decided to use the
[`targets`](https://cran.r-project.org/package=targets) package to
manage the workflow and the dependencies between the different steps of
the analysis. Therefore, the code is contained in the `_targets.R` file
and the results are summarised in the `results.html` file which will be
created by `tar_make()` command inside the `reports` folder at the end
of the workflow. You can check the `targets` package
[documentation](https://books.ropensci.org/targets/) for more
information. B. Executing the `tar_make()` function will download a lot
of data (~1GB) C. The code will take a long time to run (about 3 hours
on a standard laptop).

Instructions:

1.  Download or clone this repository.
2.  Open the `Rstudio` project file `ppm-fire-occurrences.Rproj`.
3.  Run the following code:

``` r
library(targets)
tar_make()
```

As already mentioned, the `tar_make()` command will execute the code
contained in the `_targets.R` file and reproduce the analyses presented
in the paper. At the end of the workflow, you will find a `results.html`
file inside the `reports` folder that includes all figures and results.

## References

Landau, W. M., (2021). The targets R package: a dynamic Make-like
function-oriented pipeline toolkit for reproducibility and
high-performance computing. Journal of Open Source Software, 6(57),
2959, <https://doi.org/10.21105/joss.02959>
