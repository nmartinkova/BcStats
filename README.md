# BcStats
Statistics for Bachelors in the Martinkova-Smolinsky lab. Currently, calculates maximum speed in a 10cm interval and fits models to count data and continuous data.

## Installation

To start using _BcStats_, install it from GitHub from within `R`. 
1. I you do not have `R` on the computer, get the `R` version suitable for your operating system from [CRAN](https://cran.r-project.org). 
1. Open the `R` console.
1. Install the *BcStats* package for the first time as:

``` r
# Check whether required package devtools exists, if not install it
if(!require("devtools", character.only = TRUE)){
    install.packages("devtools", dependencies = TRUE)
    library(devtools)
}

# Install BcStats from GitHub using devtools. Internet access is necessary
install_github("nmartinkova/BcStats", upgrade = TRUE)

# Load the BcStats package
require(BcStats)
# Loading required package: BcStats
```

## Using *BcStats*

Once both `R` and the *BcStats* package have been installed, open `R`, type `require(BcStats)`, and hit Enter. Check whether the functions you need are available:

``` r
help(package = "BcStats")
```

When the function you need is not available, try the installation code above again and recheck.

Contact your supervisor on MS Teams if the instructions in the help are unclear.