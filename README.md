
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Multi-action conservation planning <img src="man/figures/logo.png" align="right" width=15% />

<!-- badges: start -->

![CRAN/METACRAN](https://www.r-pkg.org/badges/version/prioriactions)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CodeFactor](https://www.codefactor.io/repository/github/prioriactions/prioriactions/badge)](https://www.codefactor.io/repository/github/prioriactions/prioriactions)
<!-- badges: end -->

**This project was financed by the National Agency of Research and
Development, ANID, Chile, through the grant FONDECYT N.1180670 and
through the Complex Engineering Systems Institute PIA/BASAL AFB180003.
Also it has received funding from the European Union’s H2020 research
and innovation program under the Marie Sklodowska-Curie grant agreement
N.691149 (SuFoRun).**

## Overview

The `prioriactions` package allows you to create and solve conservation
planning problems that involve multiple threats and actions. This uses
techniques of integer linear programming (ILP), obtaining optimal
solutions or with a certain degree of guaranteed quality (gap). Due to
its flexibility, the package offers the possibility of creating
different mathematical models with multiple requirements (spatial,
budget requirements, etc.). All the included models are presented in
detail in Salgado-Rojas et al. (2020). The package has a variety of
commercial and open-source exact algorithm solvers that guarantee to
find optimal solutions.

## Installation

The latest development version of `prioriactions` can be installed from
[GitHub](https://github.com/prioriactions/prioriactions/) using the
following code.

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("prioriactions/prioriactions")
```

## Usage

You can browse the package documentation online at
<https://prioriactions.github.io/prioriactions/>.

If this is your first time using `prioriactions`, we strongly recommend
reading the [Introduction to
prioriactions](https://prioriactions.github.io/prioriactions/articles/prioriactions.html)
vignette.

If you believe you’ve found a bug in `prioriactions`, please file a bug
(and, if possible, a [reproducible
example](https://reprex.tidyverse.org)) at
<https://github.com/prioriactions/prioriactions/issues>.

## References

-   Salgado-Rojas J, Alvarez-Miranda E, Hermoso V, Garcia-Gonzalo J,
    Weintraub A. *A mixed integer programming approach for multi-action
    planning for threat management*. Ecological Modelling 2020;
    418:108901. DOI: <https://doi.org/10.1016/j.ecolmodel.2019.108901>.
