#' @title Get total cost
#'
#' @description Provides the sum of costs to do actions and  select planning units
#'in a planning exercise. This is the sum of value returned from `getActionsCost()` function and
#'the value returned from `getPlanningUnitsCost()` function.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' @return [numeric].
#'
#' @seealso `getActionsCost()` and `getPlanningUnitsCost()`.
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
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data, sensitivity = sim_sensitivity_data,
#'   boundary = sim_boundary_data
#' )
#'
#' ## Create optimization model
#' problem_model <- minimizeCosts(x = problem_data, blm = 1)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, solver = "gurobi", gap_limit = 0.01, output_file = FALSE)
#'
#' # get total cost of a solution
#' getTotalCost(s)
#'
#' @name getTotalCost
NULL

#' @rdname getTotalCost
#' @export
getTotalCost <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  return(getActionsCost(x) + getPlanningUnitsCost(x))
}
