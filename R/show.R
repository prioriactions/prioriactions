#' @include internal.R
NULL

#' @title Show
#'
#' @description Displays information about an object.
#'
#' @param x Any object.
#' @param ... Not used.
#'
#' @name show
#'
#' @return None.
#'
#' @seealso [methods::show()].
#'
#' @aliases show, ConservationProblem-method show, OptimizationProblem-method show, Solution-method show
#'
#' @examples
#' a <- 1:4
#' show(a)
NULL

#' @rdname show
#' @method show Data
#' @export
show.Data <- function(x, ...) x$show()

#' @rdname show
#' @method show OptimizationProblem
#' @export
show.OptimizationProblem <- function(x, ...) x$show()

#' @rdname show
#' @method show Solution
#' @export
show.Solution <- function(x, ...) x$show()

#' @rdname show
#' @method show Portfolio
#' @export
show.Portfolio<- function(x, ...) x$show()

