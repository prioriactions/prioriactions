## R CMD check results

0 errors | 0 warnings | 3 notes

## Resubmission

* Please explain in your description text how to get this software. 
  - Done

* A CRAN package should not be larger than 5 MB. Please reduce the size. 
  - Done

* If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
  authors (year) <doi:...>
  authors (year) <arXiv:...>
  authors (year, ISBN:...)
  or if those are not available: <https:...>
  - Done

* Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation.
  - Done

* \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user.
  - Done

* Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.
  - Done

* Please ensure that you do not use more than 2 cores in your examples, vignettes, etc. 
  - Done
  
## CRAN check notes

* checking CRAN incoming feasibility ... NOTE
  - Maintainer: ‘Jose Salgado-Rojas <jose.salgado.rojas@hotmail.com>’
  
  **New submission**
 
* checking package dependencies ... NOTE
  Package suggested but not available for checking: ‘gurobi’

  **This software is not yet available in CRAN, but it is perfectly documented on their website (https://www.gurobi.com/documentation/9.1/refman/r_api_overview.html). In addition, there are comprehensive instructions for its installation in the [`gurobi installation` vignette](https://prioritizr.net/articles/gurobi_installation.html) of the prioritizr package (referenced appropriately in prioriactions package)**

* checking installed package size ... NOTE
  - installed size is 32.2Mb
  - sub-directories of 1Mb or more:
      doc   3.3Mb
      libs  17.5Mb

## Test environments

 - GitHub Actions (ubuntu-20.04): oldrel-1, release, devel
 - GitHub Actions (windows): release
 - Github Actions (macOS): release

## Downstream dependencies

There are no existing packages that depend on this package.
