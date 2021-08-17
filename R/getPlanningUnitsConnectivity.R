#' @title Get connectivity of planning units
#'
#' @description Provides the connectivity value of all planning units in a solution.
#'
#' @param x `Solution-class` or `Portfolio-class` object.
#'
#' @details The planning unit connectivity is calculated as the sum of all
#' boundaries unreached by the planning units in the solution. This can be expressed
#' mathematically for a set of planning units
#'\eqn{I} indexed by \eqn{i} and \eqn{j}, and
#'a set of threats \eqn{K} indexed by \eqn{k} as:
#'
#' \deqn{
#' \sum_{i \in I}\sum_{j \in I} w_{i} (1 - w_{j})cv_{ij}
#' }
#'
#' Here, \eqn{w_{i}} is the decisions variable that specify whether the planning
#' unit \eqn{i} has been selected (1) or not (0), \eqn{cv_{ij}}
#' is the large of boundary that share the planning units \eqn{i} and \eqn{j}.
#' If is a `recovery` model,
#' then all planning unit selected have at least one action to perform on it.
#' More information about of recovery models in
#' `minimizeCosts()` and `maximizeBenefits()` functions.
#'
#' @return [numeric].
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
#' # get connectivity of units
#' getUnitConnectivity(s)
#'
#' @name getPlanningUnitsConnectivity
NULL

#' @rdname getPlanningUnitsConnectivity
#' @export
getPlanningUnitsConnectivity <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  if(inherits(x, "Solution")){
    boundary_data <- x$OptimizationClass$ConservationClass$data$boundary

    if(is.null(boundary_data)){
      return(NA)
    }
    else{
      solution_units <- getPlanningUnits(x)$solution

      if(!(getStatusCode(x) %in% c(1,3))){
        connectivity <- rcpp_stats_connectivity_units(x$OptimizationClass$ConservationClass$data$pu,
                                                      boundary_data,
                                                      x$OptimizationClass$ConservationClass$data$dist_threats,
                                                      x$OptimizationClass$ConservationClass$data$dist_features,
                                                      x$data$sol)
        return(sum(connectivity))
      }
      else{
        return(NA)
      }
    }
  }
  else if(inherits(x, "Portfolio")){

    return_list <- NULL

    for(it in seq_len(length(x$data))){

      out <- getPlanningUnitsConnectivity(x$data[[it]])

      return_list <- c(return_list, out)
    }
    return(return_list)
  }
}
