
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveyspec

<!-- badges: start -->

[![R-CMD-check](https://github.com/tmelliott/surveyspec/workflows/R-CMD-check/badge.svg)](https://github.com/tmelliott/surveyspec/actions)
[![Codecov test
coverage](https://codecov.io/gh/tmelliott/surveyspec/branch/main/graph/badge.svg)](https://codecov.io/gh/tmelliott/surveyspec?branch=main)

<!-- badges: end -->

The goal of surveyspec is to …

## Installation

You can install the released version of surveyspec from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("surveyspec")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tmelliott/surveyspec")
```

## Example

There are two audiences for ‘surveyspec’, those who collect and
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
dclus2_spec <- import_survey('apiclus2.svydesign', data = apiclus2)
dclus2_spec
#> Survey design specification:
#>  * ids: dnum + snum
#>  * fpc: fpc1 + fpc2
#>  * type: survey
#> 
#> Design object: 
#> 2 - level Cluster Sampling design
#> With (40, 126) clusters.
#> survey::svydesign(ids = ~dnum + snum, fpc = ~fpc1 + fpc2, data = data)

dclus2_spec$design
#> 2 - level Cluster Sampling design
#> With (40, 126) clusters.
#> survey::svydesign(ids = ~dnum + snum, fpc = ~fpc1 + fpc2, data = data)
```

### Including data with the specification

In many cases, it makes sense to distribute the specification along with
the data. In this case, the specification file can include a line `data
= relative/path/to/data.csv`. It is generally advised to place these in
the same folder, which can be zipped and distributed. Users can then
extract both files at once into the same directory and get going. To
load a file with data, you’ll need to specify a function to import the
file.

``` r
spec <- import_survey('somespec.svydesign', read_fun = read.csv)
```
