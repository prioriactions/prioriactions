#' @include internal.R
NULL

#' Show
#'
#' Display information about an object.
#'
#' @param x Any object.
#'
#' @seealso [methods::show()].
#'
#' @name show
#'
#' @aliases show,Data-method show,OptimizationProblem-method show,Solution-method show,Portfolio-method show
NULL

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{Data}(x)
#'
methods::setMethod("show", "Data",
                   function(object) object$show())

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{OptimizationProblem}(x)
#'
methods::setMethod("show", "OptimizationProblem",
                   function(object) object$show())

#' @name show
#'
#' @rdname show
#'
#' @usage \S4method{show}{Portfolio}(x)
#'
methods::setMethod("show", "Portfolio",
                   function(object) object$show())
