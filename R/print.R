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
#' @seealso \code{\link[base]{print}}.
#'
#' @aliases print
#'
#' @examples
#' a <- 1:4
#' print(a)
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
#' @method print Portafolio
#' @export
print.Portafolio <- function(x, ...) x$print()
