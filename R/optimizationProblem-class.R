#' @include internal.R

#' @export
if (!methods::isClass("OptimizationProblem")) methods::setOldClass("OptimizationProblem")
NULL

#' Optimization problem class
#'
#' This class is used to represent an optimization model. This includes several
#' methods for obtaining model information. It is created used models functions
#' (i.e. `minimizeCosts()` and `maximizeBenefits()` functions).
#'
#' @section Fields: \describe{ \item{$data}{`list` object containing data
#'   of the mathematical model.}
#'
#'   \item{$ConservationClass}{object of class
#'   [conservationProblem-class()] that contain the data instance.}
#'
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
#'   [vector()]. Object stored in the `data`. It correspond to set
#'   of data stored associated to the mathematical model.}
#'
#'   \item{getModelSense()}{
#'   `character`. Indicate whether the
#'   model is minimization or maximization.}
#'
#'   \item{getNcol()}{
#'   `integer`. Number indicating the columns of matrix A.}
#'
#'   \item{getNrow()}{
#'   `integer`. Number indicating the rows of matrix A.}
#'
#'   \item{getSizeA()}{
#'   `character`. Number indicating the size of matrix A (in kilo Bytes).}
#'
#'   \item{print()}{
#'   Print basic information of the optimization model.}
#'
#'   \item{show()}{
#'   Call print method.}
#'
#'   }
#'
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
#' problem_data <- problem(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create optimization model
#' problem_model <- minimizeCosts(x = problem_data, blm = 1)
#'
#' ## Use class methods
#' head(problem_model$getData("obj"))
#'
#' head(problem_model$getDataList()[7])
#'
#' problem_model$getModelSense()
#'
#' problem_model$getNconstraints()
#'
#' problem_model$getNvariables()
#'
#' problem_model$getSizeA()
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
    if (getNvariables(self) > 0) {
      message(
        "Optimization Problem",
        "\n  model sense: ", getModelSense(self),
        "\n  dimensions:  ", getNconstraints(self), ", ", getNvariables(self), ", ", getSizeA(self), " Mb",
        " (nrow, ncol, size)",
        "\n  variables:   ", getNvariables(self)
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
