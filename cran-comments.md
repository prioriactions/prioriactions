## R CMD check results

0 errors | 0 warnings | 3 notes

## Submission

* New version released. Now, the problem() function works with all types of planning objectives.
  
## CRAN check notes

* checking CRAN incoming feasibility ... NOTE
  - Maintainer: ‘Jose Salgado-Rojas <jose.salgroj@gmail.com>’
  
  **New submission**
 
* checking package dependencies ... NOTE
  Package suggested but not available for checking: ‘gurobi’ and ‘cplexAPI’

  **This software is not yet available in CRAN, but it is perfectly documented on their website (https://www.gurobi.com/documentation/9.1/refman/r_api_overview.html). In addition, there are comprehensive instructions for its installation in the [`gurobi installation` vignette](https://prioritizr.net/articles/gurobi_installation.html) of the prioritizr package (referenced appropriately in prioriactions package)**

* checking installed package size ... NOTE
  - installed size is 20.8Mb
  - sub-directories of 1Mb or more:
      doc   3.4Mb
      libs  16.3Mb

## Test environments

 - GitHub Actions (ubuntu-20.04): oldrel-1, release, devel
 - GitHub Actions (windows): release
 - Github Actions (macOS): release

## Downstream dependencies

There are no existing packages that depend on this package.
