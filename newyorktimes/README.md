
<!-- README.md is generated from README.Rmd. Please edit that file -->

# newyorktimes

<!-- badges: start -->

<!-- badges: end -->

The newyorktimes pacakge is an interface to get the information from the
New York Times.

## Authentication

Get your own API keys for NYTimes APIs at
<http://developer.nytimes.com/signup>

Then, you need put your own API keys in your `.Renviron` file:

#### 1\. open your `.Renviron` file

``` r
R.home(component = "home")
usethis::edit_r_environ()
```

#### 2\. write your API keys

NYT\_KEY=<your_API_key>

## Before Installation

You need to install the following packages to make sure the use of the
newyorktimes package.  
httr, stringr, plyr, tidyr, jsonlite, dplyr, xml2

## Installation

You can install the released version of newyorktimes from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("newyorktimes")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("QMSS-G5072-2019/Gao_Fengyi")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(newyorktimes)
# use search_article() function to find articles' information 
search_article('new york', 20191231)
```
