## R CMD check results

0 errors | 0 warnings | 4 notes

## CRAN check notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Jose Salgado-Rojas <jose.salgado.rojas@hotmail.com>’
  
 **New submission**
 
* checking package dependencies ... NOTE
  Package suggested but not available for checking: ‘gurobi’

  **The _gurobi_ provide interfaces to optimization software. Although they are not available on CRAN, we provide instructions for installing these R packages in the DESCRIPTION file and the package documentation (see `?add_gurobi_solver`, `?add_cbc_solver`). Additioanlly, comprehensive instructions for installing the _gurobi_ R package are also provided in the Gurobi Installation Guide vignette (see `vignette('gurobi_installation', package = "prioritizr")`).**

* Package was archived on CRAN

  CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2021-10-06 as check problems were not
    corrected in time.

  **As previously mentioned, this version has been updated to fix the issues that resulted in archival.**

* Possibly misspelled words in DESCRIPTION:
  Gurobi (16:49, 18:5)
  MILP (7:18)
  optimality (12:5)
  pre (11:62)

  **These words are spelled correctly.**

* checking package dependencies ... NOTE
  Packages suggested but not available for checking: 'gurobi', 'rcbc'

  **The _gurobi_ and _rcbc_ R packages provide interfaces to optimization software. Although they are not available on CRAN, we provide instructions for installing these R packages in the package documentation (see `?add_gurobi_solver`, `?add_cbc_solver`). Comprehensive instructions for installing the _gurobi_ R package are also provided in the Gurobi Installation Guide vignette (see `vignette('gurobi_installation', package = "prioritizr")`). The DESCRIPTION file also provides information on installing these R packages.**

## Previous notes from CRAN maintainers

* Thanks, please omit the redundant "in R".

  **Thank you for this suggestion! The prioritizr R package was first released on CRAN in 2016. Since then it has been cited in nearly 30 scientific publications (see the publication record vignette), and has a worldwide community spanning over 100 countries (based on website tracking analytics). Although the "in R" text in the title is redundant, we worry that changing the title at this point in time would confuse the user base and invalidate previous work that has cited this package. As such, we would prefer to keep the current title.**

## Test environments

* [Ubuntu 20.04, R-release](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AUbuntu)
* [Ubuntu 20.04, R-devel](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/prioritizr/prioritizr/actions?query=workflow%3A%22Mac+OSX%22)
* [Windows Server 2019, R-release](https://github.com/prioritizr/prioritizr/actions?query=workflow%3AWindows)
* Windows Server 2008 (x64), R-devel (win-builder)

## Downstream dependencies

There are no existing packages that depend on this package.
