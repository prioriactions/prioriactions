#' @include internal.R
NULL

#' @title Print
#'
#' @description Display information about an object.
#'
#' @param x Any object.
#' @param ... Not used.
#'
#' @name print
#'
#' @return None.
#'
#' @seealso [base::print()].
#'
#' @aliases print

NULL

#' @rdname print
#' @method print ConservationProblem
#' @export
print.ConservationProblem <- function(x, ...) x$print()

#' @rdname print
#' @method print OptimizationProblem
#' @export
print.OptimizationProblem <- function(x, ...) x$print()

#' @rdname print
#' @method print Solution
#' @export
print.Solution <- function(x, ...) x$print()

#' @rdname print
#' @method print Portfolio
#' @export
print.Portfolio <- function(x, ...) x$print()
