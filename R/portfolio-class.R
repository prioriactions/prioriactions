#' @include internal.R

#' @export
if (!methods::isClass("Portfolio")) methods::setOldClass("Portfolio")
NULL

#' Portfolio class
#'
#' This class is used to represent multiple solutions of the MIP (Mixed-Integer Programming) model
#' related to the multi-action conservation planning problem. This includes several methods
#' to obtain information about both the optimization process and the solution associated with
#' the planning units and conservation actions. It is created using the *eval* functions
#' (e.g. `evalTarget()` or `evalBudget()`).
#'
#' @section Fields:
#' \describe{
#' \item{$data}{
#' `list`. Object containing data on the results of the optimization process.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{getGap()}{
#' `numeric`. Label indicating the optimality gap achieved for the MIP model.}
#'

#' \item{getObjectiveValue()}{
#' `numeric`. Number indicating the value of the objective function of the solution.}
#'
#' \item{print()}{
#' Print basic information of the model solution.}
#'
#' \item{show()}{
#' Call print method.}
#'
#' \item{plot()}{
#' Call plot method.}
#' }
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' inputs <- list(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create model and solve
#' port <- evalTarget(data = inputs, prop = c(0.5, 0.6), gap_limit = 0.01, output_file = FALSE)
#'
#' ## Use class methods
#' port$getGap()
#'
#' port$getObjectiveValue()
#'
#' port$print()
#'
#' port$plot()
#'
#' @name portfolio-class
#'
#' @aliases Portfolio
NULL

#' @export
Portfolio <- pproto(
  "Portfolio",
  data = list(),
  print = function(self) {
    message(
      "Portfolio overview",
      "\n  solutions: ", length(self$data)
    )
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "Portfolio object"
  },
  getNames = function(self) {
    return_list <- NULL

    for(it in seq_len(length(self$data))){

      out <- self$data[[it]]$name

      return_list <- c(return_list, out)
    }

    return(return_list)
  },
  getBlms = function(self) {
    return_list <- NULL

    for(it in seq_len(length(self$data))){

      out <- self$data[[it]]$OptimizationClass$data$args$blm

      return_list <- c(return_list, out)
    }

    return(return_list)
  }
)
