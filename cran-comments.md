## R CMD check results

0 errors | 0 warnings | 3 notes

## Submission

* New version released:
  -   Now `solve()` function allows CPLEX solver.
  -   Update `inputData()` description about *d* parameter.
  -   Update documentation.
  -   Now `solve()` function exports solution scores.
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
  Packages suggested but not available for checking: 'gurobi', 'cplexAPI'

  **gurobi software is not yet available in CRAN, but it is perfectly documented on their website (https://www.gurobi.com/documentation/9.1/refman/r_api_overview.html). In addition, there are comprehensive instructions for its installation in the [`gurobi installation` vignette](https://prioritizr.net/articles/gurobi_installation.html) of the prioritizr package (referenced appropriately in prioriactions package). Instead, cplexAPI is not available in CRAN now but was a while ago (archived on 2021-11-05). We are using the updated version sourced on GitHub (https://github.com/cran/cplexAPI). In addition, there are comprehensive instructions for its installation in https://github.com/cran/cplexAPI/blob/master/inst/INSTALL. We noticed the creators are working in a new version for CRAN here (https://github.com/SysBioChalmers/sybil-cplexAPI).**

* checking installed package size ... NOTE
    installed size is  5.0Mb
    sub-directories of 1Mb or more:
      doc   3.3Mb

## Test environments

 - GitHub Actions (ubuntu-20.04): oldrel-1, release, devel
 - GitHub Actions (windows): release
 - Github Actions (macOS): release

## Downstream dependencies

There are no existing packages that depend on this package.
