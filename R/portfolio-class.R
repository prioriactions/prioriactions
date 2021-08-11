#' @include internal.R

#' @export
if (!methods::isClass("Portfolio")) methods::setOldClass("Portfolio")
NULL

#' Portfolio class
#'
#' This class is used to represent the solution of the MIP (Mixed-Integer Programming) model
#' related to the multi-action conservation planning problem. This includes several methods
#' to obtain information about both the optimization process and the solution associated with
#' the planning units and conservation actions. It is created using the [solve()]
#' function.
#'
#' @section Fields:
#' \describe{
#' \item{$data}{`list` object containing data on the results of the optimization process.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{$getGap( )}{returns a `string` label indicating the optimality gap achieved for the MIP model.}
#'

#' \item{$getObjectiveValue( )}{returns a `numeric` number indicating the value of the objective function at the optimum.}
#'
#' \item{$getSolutionActions( )}{
#' returns a `data.frame` object interpreting the optimal solution of the MIP model
#' that relates to the conservation actions. It contains information on the conservation actions
#' that are suggested (value 1) and those that are not suggested (value 0) within the conservation plan.}
#'
#' \item{$getSolutionUnits( )}{
#' returns a `data.frame` object interpreting the optimal solution of the MIP model
#' that relates to the planning units. It contains information on the planning units that are
#' suggested to be included (value 1) and not included (value 0) within the conservation plan.}
#'
#' \item{$print( )}{print basic information of the model solution.}
#'
#' \item{$show( )}{call print method.}
#'
#' }
#'
#' @examples
#' ## Examples of how to use the methods of a Solution class object.
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
#' ## Solve the optimization model using a default solver
#' model_solution <- solve(a = problem_model, verbose = FALSE)
#'
#' ## Use class methods
#' model_solution$getGap()
#'
#' model_solution$getObjectiveValue()
#'
#' head(model_solution$getSolutionActions())
#'
#' head(model_solution$getSolutionUnits())
#'
#' model_solution$print()
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
    return_list <- c()

    for(it in 1:length(self$data)){

      out <- self$data[[it]]$name

      return_list <- c(return_list, out)
    }

    return(return_list)
  },
  getBlms = function(self) {
    return_list <- c()

    for(it in 1:length(self$data)){

      out <- self$data[[it]]$OptimizationClass$data$args$blm

      return_list <- c(return_list, out)
    }

    return(return_list)
  }
)
