#' @title Get cost of actions
#'
#' @description Provides the total cost of selected actions in a solution.
#'
#' @param x [solution-class] or [portfolio-class] object.
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
#' # get action cost
#' getActionsCost(s)
#'
#' @name getActionsCost
NULL

#' @rdname getActionsCost
#' @export
getActionsCost <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  solution_actions <- getActions(x, format = "large")

  if(inherits(x, "Solution")){
    if(!(getStatusCode(x) %in% c(1,3))){
      a <- rcpp_stats_costs_actions(x$OptimizationClass$ConservationClass$data$dist_threats,
                                    c(solution_actions$solution))
      return(sum(a))
    }
    else{
      return(NA)
    }
  }
  else if(inherits(x, "Portfolio")){

    return_list <- NULL

    for(it in seq_len(length(x$data))){

      out <- getActionsCost(x$data[[it]])

      return_list <- c(return_list, out)
    }

    return(return_list)
  }
}
