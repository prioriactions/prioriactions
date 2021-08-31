#' @title Get connectivity of actions
#'
#' @description Provides the connectivity value of all actions in a solution.
#'
#' @param x `Solution-class` or `Portfolio-class` object.
#'
#' @details The connectivity among actions is calculated as the sum of all
#' connectivity penalties by each action in the solution. This can be expressed
#' mathematically for a set of planning units
#'\eqn{I} indexed by \eqn{i} and \eqn{j}, and
#'a set of threats \eqn{K} indexed by \eqn{k} as:
#'
#' \deqn{
#' \sum_{k \in K}\sum_{i \in I_k}\sum_{j \in I_k} x_{ik} (1 - x_{jk})cv_{ij}
#' }
#'
#' Where, \eqn{x_{ik}} is the decisions variable that specify
#' whether an action has been selected to abate threat \eqn{k} in planning unit
#' \eqn{i} (1) or not (0), \eqn{cv_{ij}} is the connectivity penalty that applies
#' when a solution contains planning unit \eqn{i} but not \eqn{j} o viceversa.
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
#' # get connectivity of actions
#' getActionsConnectivity(s)
#'
#' @name getActionsConnectivity
NULL

#' @rdname getActionsConnectivity
#' @export
getActionsConnectivity <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))


  if(inherits(x, "Solution")){
    boundary_data <- x$OptimizationClass$ConservationClass$data$boundary

    if(is.null(boundary_data)){
      return(NA)
    }
    else{
      solution_actions <- getActions(x, format = "large")$solution

      if(!(getStatusCode(x) %in% c(1,3))){
        connectivity <- rcpp_stats_connectivity_actions(x$OptimizationClass$ConservationClass$data$pu,
                                                        x$OptimizationClass$ConservationClass$data$threats,
                                                        x$OptimizationClass$ConservationClass$data$dist_threats,
                                                        x$OptimizationClass$ConservationClass$data$boundary,
                                                        solution_actions)
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

      out <- getActionsConnectivity(x$data[[it]])

      return_list <- c(return_list, out)
    }
    return(return_list)
  }
}
