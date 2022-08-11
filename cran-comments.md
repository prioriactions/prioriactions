## R CMD check results

0 errors | 0 warnings | 3 notes

## Submission

* New version released:
  -   Now `solve()` function allows Cplex solver via Rcplex package.
  -   Update `inputData()` description about *d* parameter.
  -   Update documentation.
  -   Now `solve()` function exports solution scores (in the parameters file).
  -   Now we sort *id's* of inputs data in `inputData()` function.

  
## CRAN check notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Jose Salgado-Rojas <jose.salgroj@gmail.com>’
  
  New maintainer:
    Jose Salgado-Rojas <jose.salgroj@gmail.com>
  Old maintainer(s):
    Jose Salgado-Rojas <jose.salgado.rojas@hotmail.com>
  
  **New submission. I changed my personal email account to jose.salgroj@gmail.com from jose.salgado.rojas@hotmail.com. I hope this is not a problem.**
 
* checking package dependencies ... NOTE
  Packages suggested but not available for checking: 'gurobi', 'Rcplex'

  **gurobi software is not yet available in CRAN, but it is perfectly documented on their website (https://www.gurobi.com/documentation/9.1/refman/r_api_overview.html). In addition, there are comprehensive instructions for its installation in the [`gurobi installation` vignette](https://prioritizr.net/articles/gurobi_installation.html) of the prioritizr package (referenced appropriately in prioriactions package). Instead, cplex software is available in CRAN via the Rcplex package. However, has certain steps to do it completely available in our package. It is because this is a commercial MIP solver (like gurobi), and we need to get an academic license before its installation. Later after installing cplex, we can follow [this guide](https://github.com/cran/Rcplex/blob/master/inst/INSTALL) to install Rcplex package. All of this is fully documented in our [solve function reference](https://prioriactions.github.io/prioriactions/reference/solve.html).**

* checking installed package size ... NOTE
     installed size is 11.4Mb
     sub-directories of 1Mb or more:
       doc    3.3Mb
       libs   6.9Mb

## Test environments

 - GitHub Actions (ubuntu-20.04): oldrel-1, release, devel
 - GitHub Actions (windows): release
 - Github Actions (macOS): release

## Downstream dependencies

There are no existing packages that depend on this package.
