## R CMD check results

0 errors | 0 warnings | 3 notes

## Submission

* New version released:
  -   Update `inputData()` function reference.
  -   Now parameters *a*, *b*, *c*, *d* for sensitivity input are called delta1, *delta2*, *delta3*, *delta4* respectively.
  -   Fixed warning message with `show()` on linux.

  
## CRAN check notes

* checking package dependencies ... NOTE
  Packages suggested but not available for checking: 'gurobi', 'Rcplex'

  **gurobi software is not yet available in CRAN, but it is perfectly documented on their website (https://www.gurobi.com/documentation/9.1/refman/r_api_overview.html). In addition, there are comprehensive instructions for its installation in the [`gurobi installation` vignette](https://prioritizr.net/articles/gurobi_installation.html) of the prioritizr package (referenced appropriately in prioriactions package). Instead, cplex software is available in CRAN via the Rcplex package. However, has certain steps to do it completely available in our package. It is because this is a commercial MIP solver (like gurobi), and we need to get an academic license before its installation. Later after installing cplex, we can follow [this guide](https://github.com/cran/Rcplex/blob/master/inst/INSTALL) to install Rcplex package. All of this is fully documented in our [solve function reference](https://prioriactions.github.io/prioriactions/reference/solve.html).**

❯ checking installed package size ... NOTE
    installed size is 15.5Mb
    sub-directories of 1Mb or more:
      doc    3.3Mb
      libs  10.9Mb

## Test environments

 - GitHub Actions (ubuntu-20.04): oldrel-1, release, devel
 - GitHub Actions (windows): release
 - Github Actions (macOS): release

## Downstream dependencies

There are no existing packages that depend on this package.
