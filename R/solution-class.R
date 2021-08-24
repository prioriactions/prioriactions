#' @include internal.R

#' @export
if (!methods::isClass("Solution")) methods::setOldClass("Solution")
NULL

#' Solution class
#'
#' This class is used to represent the solution of the MIP (Mixed-Integer Programming) model
#' related to the multi-action conservation planning problem. This includes several methods
#' to obtain information about both the optimization process and the solution associated with
#' the planning units and conservation actions. It is created using the [solve()]
#' function.
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
#' }
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
#' ## Solve the optimization model
#' s <- solve(a = problem_model, solver = "gurobi", gap_limit = 0.01, output_file = FALSE)
#'
#' ## Use class methods
#' s$getGap()
#'
#' s$getObjectiveValue()
#'
#' s$print()
#'
#' @name solution-class
#'
#' @aliases Solution
NULL

#' @export
Solution <- pproto(
  "Solution",
  data = list(),
  OptimizationClass = NULL,
  name = "sol",
  print = function(self) {
    message(
      "Solution overview",
      "\n  name: ", self$name,
      "\n  objective value: ", getObjectiveValue(self),
      "\n  gap:  ", getGap(self),
      "\n  status:  ", getStatus(self),
      "\n  runtime: ", paste0(getTimeSolving(self), " sec")
    )
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "Solution object"
  }
)
