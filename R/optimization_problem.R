#' @include internal.R

#' @export
if (!methods::isClass("OptimizationProblem")) methods::setOldClass("OptimizationProblem")
NULL

#' Optimization problem class
#'
#' This class is used to represent an optimization model. This includes several
#' methods for obtaining model information. It is created used models functions
#' (e.g. min costs).
#'
#' @section Fields: \describe{ \item{$data}{\code{list} object containing data
#'   of the mathematical model.}
#'
#'   \item{$ConservationClass}{object of class
#'   \code{\link{ConservationProblem-class}} that contain the data instance.}
#'
#'   }
#'
#' @section Methods: \describe{ \item{$getData(\code{character} name)}{return an
#'   \code{\link{vector}} object stored in the \code{data} field with the
#'   corresponding \code{name}. The data correspond to the different parts of
#'   the mathematical model. The argument \code{name} can be made to the
#'   following: "obj", "rhs", "sense", "vtype", "A", "bounds" or "modelsense".}
#'
#'   \item{$getDataList( )}{returns an \code{\link{list}} of
#'   \code{\link{vector}} object stored in the \code{data}. It correspond to set
#'   of data stored asociated to the mathematical model.}
#'
#'   \item{$getModelSense( )}{returns a \code{character} indicating whether the
#'   model is minimization or maximization.}
#'
#'   \item{$getNcol( )}{returns an \code{integer} number indicating the columns
#'   of matrix A.}
#'
#'   \item{$getNrow( )}{returns an \code{integer} number indicating the rows of
#'   matrix A.}
#'
#'   \item{$getSizeA( )}{returns a \code{double} number indicating the size of
#'   matrix A (in Megabytes).}
#'
#'   \item{$print( )}{print basic information of the optimization model.}
#'
#'   \item{$show( )}{call print method.}
#'
#'   }
#'
#' @examples
#' ## Examples of how to use the methods of a OptimizationProblem class object.
#'
#' ## Load data
#' data(example_pu_data, example_features_data, example_rij_data, example_threats_data, example_sensitivity_data, example_bound_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = example_pu_data, features = example_features_data, rij = example_rij_data,
#'   threats = example_threats_data, sensitivity = example_sensibility_data,
#'   bound = example_bound_data
#' )
#'
#' ## Create optimization model
#' problem_model <- min_costs(problem_data, blm = 1, blm_actions = 1)
#'
#' ## Use class methods
#' head(problem_model$getData("obj"))
#'
#' head(problem_model$getDataList()[7])
#'
#' problem_model$getModelSense()
#'
#' problem_model$getNcol()
#'
#' problem_model$getNrow()
#'
#' problem_model$getSizeA()
#'
#' problem_model$print()
#' @name OptimizationProblem-class
#'
#' @aliases OptimizationProblem
NULL

#' @export
OptimizationProblem <- pproto(
  "OptimizationProblem",
  data = list(),
  ConservationClass = NULL,
  print = function(self) {
    if (self$getNcol() > 0) {
      message(
        "Optimization Problem",
        "\n  model sense: ", self$getModelSense(),
        "\n  dimensions:  ", self$getNrow(), ", ", self$getNcol(), ", ", self$getSizeA(), " Mb",
        " (nrow, ncol, size)",
        "\n  variables:   ", self$getNcol(),
        "\n  building time: ", paste0(self$getTimeBuildingModel(), " sec")
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
  getNcol = function(self) {
    return(base::ncol(self$data$A))
  },
  getNrow = function(self) {
    return(base::nrow(self$data$A))
  },
  getModelSense = function(self) {
    if (self$data$modelsense == "min") {
      return("minimization")
    }
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
  },
  getSizeA = function(self) {
    size_A <- utils::object.size((self$data$A)) / (1024 * 1024)
    return(base::round(size_A, 3))
  },
  getTimeBuildingModel = function(self) {
    return(base::round(as.numeric(self$data$statistics$timer[13]), 2))
  },
)
