#' @include internal.R

#' @export
if (!methods::isClass("OptimizationProblem")) methods::setOldClass("OptimizationProblem")
NULL

#' Optimization problem class
#'
#' This class encodes the corresponding optimization model. It is created
#' using `problem()` function.
#'
#' @section Fields: \describe{
#'
#'   \item{$data}{`list` object containing data
#'   of the mathematical model.}
#'
#'   \item{$ConservationClass}{object of class
#'   [data-class()] that contains the data input.}
#'   }
#'
#' @section Methods: \describe{
#'  \item{getData(`character` name)}{
#'   [vector()]. Object stored in the `data` field with the
#'   corresponding `name`. The data correspond to the different parts of
#'   the mathematical model. The argument `name` can be made to the
#'   following: "obj", "rhs", "sense", "vtype", "A", "bounds" or "modelsense".}
#'
#'   \item{getDataList()}{
#'    [list()] of
#'   [vector()]. Object stored in the `data`. It contains all information relative
#'   to the mathematical model, such as "obj", "rhs", etc.}
#'
#'   \item{print()}{
#'   Print basic information of the optimization model.}
#'
#'   \item{show()}{
#'   Call print method.}
#'
#'   }
#' @return No return value.
#' @examples
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create data instance
#' problem_data <- inputData(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create optimization model
#' problem_model <- problem(x = problem_data, blm = 1)
#'
#' ## Use class methods
#' head(problem_model$getData("obj"))
#'
#' problem_model$print()
#' @name optimizationProblem-class
#'
#' @aliases OptimizationProblem
NULL

#' @export
OptimizationProblem <- pproto(
  "OptimizationProblem",
  data = list(),
  ConservationClass = NULL,
  print = function(self) {
    if (getModelInfo(self)$n_variables > 0) {
      message(
        "Optimization Problem",
        "\n  model sense: ", getModelInfo(self)$model_sense,
        "\n  dimensions:  ", getModelInfo(self)$n_constraints, ", ", getModelInfo(self)$n_variables, ", ", getModelInfo(self)$size,
        " (nrow, ncol, size)",
        "\n  variables:   ", getModelInfo(self)$n_variables
      )
    } else {
      message("optimization problem (empty)")
    }
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "OptimizationProblem object"
  },
  getData = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x))
    if (!x %in% names(self$data)) {
      return(paste0("x object do not found"))
    }
    return(self$data[[x]])
  },
  getDataList = function(self) {
    return(self$data)
  }
)
