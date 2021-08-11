## usethis namespace: start
#' @useDynLib prioriactions, .registration = TRUE
## usethis namespace: end
NULL

#' @title The \pkg{prioriactions} R package
#'
#' @description The \pkg{prioriactions R} package uses a mixed integer
#'  mathematical programming (MIP) approach for building and solving
#'  multi-action conservation planning problems, where the goal is to find an
#'  optimal combination of management actions that abate threats, in an
#'  efficient way while accounting for connectivity. Furthermore, the
#'  flexibility of the package interface allows implementing an extended version
#'  of the base model for minimizing fragmentation between different actions.
#'  These models were called as MAMP and MAMP-E by Salgado-Rojas *et al.*
#'  (2020), where you can get a detailed description of how both problems were
#'  mathematically modeled. \cr
#'
#'  Once built a multi-action conservation problem, the next step is to solve
#'  it. For this, the package has a variety of commercial and open-source exact
#'  algorithm solvers that guarantee to find optimal solutions. The models
#'  implemented here have three advantages over their heuristic counterparts:
#'  shorter execution times, higher solutions quality, and a solution quality
#'  guarantee.
#'
#' @details **Put details here!**
#'
#' @references \itemize{ \item Salgado-Rojas J, <U+00C1>lvarez-Miranda E, Hermoso V,
#' Garcia-Gonzalo J, Weintraub A. *A mixed integer programming approach for
#' multi-action planning for threat management*. Ecological Modelling 2020;
#' 418:108901. \cr (DOI: <https://doi.org/10.1016/j.ecolmodel.2019.108901>) }
#'
#' @name prioriactions
#' @docType package
NULL
