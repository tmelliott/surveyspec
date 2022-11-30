
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveyspec

<!-- badges: start -->

[![R-CMD-check](https://github.com/tmelliott/surveyspec/workflows/R-CMD-check/badge.svg)](https://github.com/tmelliott/surveyspec/actions)
[![Codecov test
coverage](https://codecov.io/gh/tmelliott/surveyspec/branch/main/graph/badge.svg)](https://codecov.io/gh/tmelliott/surveyspec?branch=main)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![CRAN](https://www.r-pkg.org/badges/version/surveyspec)](https://CRAN.R-project.org/package=surveyspec)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The **surveyspec** package is designed to make it a little easier for
users to work with survey data in R—particularly those new to working
with survey designs. This is done by providing a set of helper functions
that allow survey data to be accompanied by a specification file which
includes information about stratification, clustering, and so on. This
way, there is less chance for surveys to be incorrectly specified to R,
which can result in incorrect uncertainty estimates, potentially leading
to invalid conclusions.

## Installation

<!-- You can install the released version of surveyspec from [CRAN](https://CRAN.R-project.org) with:

```r
install.packages("surveyspec")
``` -->

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tmelliott/surveyspec")
```

## Example

There are two audiences for **surveyspec**, those who collect and
distribute survey data, and those who wish to analyse them.

### Creating a survey specification file

You may specify the file manually, or if you are more familiar with the
‘survey’ package (or have the R code ready) you can simply write a
specification based on it:

``` r
library(surveyspec)
library(survey)
data(api)

dclus2 <- svydesign(~dnum+snum, fpc=~fpc1+fpc2, data = apiclus2)
write_spec(dclus2, file = 'apiclus2.svydesign')

cat(readLines('apiclus2.svydesign'), sep = "\n")
#> ids = "dnum + snum"
#> fpc = "fpc1 + fpc2"
#> type = "survey"
```

### Importing a survey specification file

To import a survey specification, you’ll need the data either loaded, or
linked from the survey specification file (see below). Then, to import
the survey design,

``` r
library(surveyspec)

dclus2_spec <- import_survey('apiclus2.svydesign', data = apiclus2)
dclus2_spec
#> Survey design specification:
#>  * ids: dnum + snum
#>  * fpc: fpc1 + fpc2
#>  * type: survey
#>  * survey_type: survey
#>  * calfun: linear
#> 
#> Design object: 
#> 2 - level Cluster Sampling design
#> With (40, 126) clusters.
#> survey::svydesign(ids = ~dnum + snum, fpc = ~fpc1 + fpc2, data = data)

dclus2_spec$design
#> 2 - level Cluster Sampling design
#> With (40, 126) clusters.
#> survey::svydesign(ids = ~dnum + snum, fpc = ~fpc1 + fpc2, data = data)

library(survey)
svymean(~api00, dclus2_spec$design)
#>         mean     SE
#> api00 670.81 30.099
```

### Including data with the specification

In many cases, it makes sense to distribute the specification along with
the data. In this case, the specification file can include a line
`data = relative/path/to/data.csv`. It is generally advised to place
these in the same folder, which can be zipped and distributed. Users can
then extract both files at once into the same directory and get going.
To load a file with data, you’ll need to specify a function to import
the file.

``` r
spec <- import_survey('somespec.svydesign', read_fun = read.csv)
```

## Working with other packages

The **surveyspec** package was initially developed as a part of
[‘iNZightTools’](https://github.com/iNZightVIT/iNZightTools/) but I
extracted it into its own package to reduce dependencies. However,
particularly for those new to R, the **iNZightTools** package provides a
useful data import function which can really make reading survey data
from a variety of sources easier, including with the addition of
metadata to describe value types:

``` r
# install.packages("iNZightTools")
write.csv(apiclus2, 'apiclus2.csv', quote = FALSE, row.names = FALSE)

# no need to specify `read_fun` if 'iNZightTools' is installed
dclus2_spec <- import_survey('apiclus2.svydesign', data = 'apiclus2.csv')
```

Additionally, much of the data manipulation within **iNZightTools**
works thanks to [**srvyr**](https://github.com/gergness/srvyr), so of
course you can work **surveyspec** into your **tidyverse** or **dplyr**
workflow:

``` r
library(dplyr)
library(srvyr)

import_survey('apiclus2.svydesign', data = 'apiclus2.csv') %>%
  as_survey() %>%
  group_by(stype) %>%
  summarize(mean_api = survey_mean(api00))
#> # A tibble: 3 × 3
#>   stype mean_api mean_api_se
#>   <fct>    <dbl>       <dbl>
#> 1 E         693.        29.9
#> 2 H         598.        17.7
#> 3 M         642.        45.1
```

## Acknowledgements

This package has been built as part of [Te Rourou
Tātaritanga](https://terourou.org), an informatics research project
looking to improve access to data in Aoteroa, New Zealand and around the
world. The project is funded by an [MBIE](https://www.mbie.govt.nz/)
Endeavour Grant, ref 62506 EDNRP.
