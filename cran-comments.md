## R CMD check results

0 errors | 0 warnings | 3 notes

## Submission

* New version released:
  -   Now we add the non-academic cbc solver as option in `solve()` function.
  -   Update `solve()` function reference.
  -   Add the `CONTRIBUTING` file. 
  -   New **solver benchmarking** vignette is added.
  -   Now `blm` parameter in `problem()` is not rounded.
  -   Now the parameter `NodefileStart` is incorporated internally in the **gurobi** solver. Set in 0.5.
  -   Update vignettes.

  
## CRAN check notes

* checking package dependencies ... NOTE
  Packages suggested but not available for checking: 'gurobi', 'Rcplex', 'rcbc'

  **gurobi software is not yet available in CRAN, but it is perfectly documented on their website (https://www.gurobi.com/documentation/9.1/refman/r_api_overview.html). In addition, there are comprehensive instructions for its installation in the [`gurobi installation` vignette](https://prioritizr.net/articles/gurobi_installation.html) of the prioritizr package (referenced appropriately in prioriactions package). Instead, cplex software is available in CRAN via the Rcplex package. However, has certain steps to do it completely available in our package. It is because this is a commercial MIP solver (like gurobi), and we need to get an academic license before its installation. Later after installing cplex, we can follow [this guide](https://github.com/cran/Rcplex/blob/master/inst/INSTALL) to install Rcplex package. All of this is fully documented in our [solve function reference](https://prioriactions.github.io/prioriactions/reference/solve.html). Similarly, the case applies to CBC. While there exists a package called rcbc that provides access to CBC functionality, ensuring its complete integration within our package involves following specific procedural guidelines. This can find a comprehensive breakdown of these steps in the documentation of the rcbc package available [here](https://dirkschumacher.github.io/rcbc/), also referenced in our [`solve()` function reference](https://prioriactions.github.io/prioriactions/reference/solve.html).**

❯ checking installed package size ... NOTE
    installed size is 16.0Mb
    sub-directories of 1Mb or more:
      doc    3.6Mb
      libs  11.1Mb

## Test environments

 - GitHub Actions (ubuntu-22.04): oldrel-1, release, devel
 - GitHub Actions (windows): release
 - Github Actions (macOS): release

## Downstream dependencies

There are no existing packages that depend on this package.
