#' @include internal.R
#'
#' @title Extract cost values
#'
#' @description Provides the sum of costs to actions and monitoring applied in a solution.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' @details The cost value is calculated as the sum of all the individual costs
#' of actions and monitoring carried out in each of the planning units. This can be expressed
#' mathematically for a set of planning units
#'\eqn{I} indexed by \eqn{i}, and
#'a set of threats \eqn{K} indexed by \eqn{k} as:
#'
#' \deqn{
#' actions = \sum_{i \in I}\sum_{k \in K_i} x_{ik} c_{ik}
#' }
#'
#' \deqn{
#' monitoring = \sum_{i \in I} x_{i \cdot} c^{'}_{i}
#' }
#' Where, \eqn{x_{ik}} is the decisions variable that specify
#' whether an action has been selected to abate threat \eqn{k} in planning unit
#' \eqn{i} (1) or not (0), \eqn{c_{ik}} is the action cost to abate threat \eqn{k}
#' in planning unit \eqn{i} and  \eqn{c^{'}_{i}} is the monitoring cost of
#' planning unit \eqn{i}. The cost of monitoring is applied to all planning units
#' where some type of action has been selected (conservation action, to abate threats
#' or connectivity).
#'
#' Note that there is an action per threat, so it is assumed that the index of
#' the threat coincides with the index of the action used to abate it.
#'
#' @return [data.frame].
#'
#' @examples
#' \donttest{
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
#' problem_model <- problem(x = problem_data)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, time_limit = 2, output_file = FALSE, cores = 2)
#'
#' ## Get costs
#' getCost(s)
#' }
#' @name getCost
NULL

#' @rdname getCost
#' @export
getCost <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  if(inherits(x, "Solution")){
    statusCode <- x$data$status

    if(!(statusCode %in% c(1,3))){
      solution_units <- x$data$sol_monitoring
      solution_actions <- x$data$sol_actions

      cost_units <- rcpp_stats_costs_units(x$OptimizationClass$ConservationClass$data$pu,
                                  solution_units)
      cost_actions <- rcpp_stats_costs_actions(x$OptimizationClass$ConservationClass$data$pu,
                                       x$OptimizationClass$ConservationClass$data$threats,
                                       x$OptimizationClass$ConservationClass$data$dist_threats,
                                       solution_actions)

      number_threats <- x$OptimizationClass$ConservationClass$getThreatsAmount()
      id_threats <- x$OptimizationClass$ConservationClass$data$threats$id

      out <- data.frame(matrix(ncol = 2 + number_threats, nrow = 0))
      col_names <- c("solution_name", "monitoring", paste0("threat_", c(id_threats)))
      colnames(out) <- col_names

      out[1, ] <- c(x$name,
                    round(cost_units, 3),
                    round(cost_actions, 3))
      return(out)
    }
    else{
      return(NA)
    }
  }
  else if(inherits(x, "Portfolio")){

    return_list <- NULL

    for(it in seq_len(length(x$data))){

      if(it == 1){
        out <- getCost(x$data[[it]])
      }
      else{
        solution_units <- x$data[[it]]$data$sol_monitoring
        solution_actions <- x$data[[it]]$data$sol_actions
        statusCode <- x$data[[it]]$data$status

        if(!(statusCode %in% c(1,3))){
          cost_units <- rcpp_stats_costs_units(x$data[[it]]$OptimizationClass$ConservationClass$data$pu,
                                               solution_units)

          cost_actions <- rcpp_stats_costs_actions(x$data[[it]]$OptimizationClass$ConservationClass$data$pu,
                                           x$data[[it]]$OptimizationClass$ConservationClass$data$threats,
                                           x$data[[it]]$OptimizationClass$ConservationClass$data$dist_threats,
                                           solution_actions)

          out[it, ] <- c(x$data[[it]]$name,
                         round(cost_units, 3),
                         round(cost_actions, 3))
        }
      }
    }
    return(out)
  }
}
