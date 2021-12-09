#' @title Extract connectivity penalty values
#'
#' @description Provides the connectivity penalty value for all actions and planning units in a solution.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' @details The connectivity penalty among is calculated as the sum of all
#' connectivity penalties by each action and planning unit in the solution. This can be expressed
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
#' problem_model <- problem(x = problem_data, blm = 0.03)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, time_limit = 2, output_file = FALSE, cores = 2)
#'
#' # get connectivity penalty values
#' getConnectivityPenalty(s)
#' }
#'
#' @name getConnectivityPenalty
NULL

#' @rdname getConnectivityPenalty
#' @export
getConnectivityPenalty <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))


  if(inherits(x, "Solution")){
    boundary_data <- x$OptimizationClass$ConservationClass$data$boundary
    statusCode <- x$data$status

    if(is.null(boundary_data)){
      return(NA)
    }
    else{
      solution_units <- x$data$sol_monitoring
      solution_actions <- x$data$sol_actions

      if(!(statusCode %in% c(1,3))){

        connectivity_units <- rcpp_stats_connectivity_units(x$OptimizationClass$ConservationClass$data$pu,
                                                      boundary_data,
                                                      x$OptimizationClass$ConservationClass$data$dist_threats,
                                                      x$OptimizationClass$ConservationClass$data$dist_features,
                                                      solution_units)

        connectivity_actions <- rcpp_stats_connectivity_actions(x$OptimizationClass$ConservationClass$data$pu,
                                                        x$OptimizationClass$ConservationClass$data$threats,
                                                        x$OptimizationClass$ConservationClass$data$dist_threats,
                                                        x$OptimizationClass$ConservationClass$data$boundary,
                                                        solution_actions)


        number_threats <- x$OptimizationClass$ConservationClass$getThreatsAmount()
        id_threats <- x$OptimizationClass$ConservationClass$data$threats$id

        #creating data.frame
        out <- data.frame(matrix(ncol = 2 + number_threats, nrow = 0))
        col_names <- c("solution_name", "units", paste0("threat_", c(id_threats)))
        colnames(out) <- col_names

        out[1, ] <- c(x$name,
                      round(connectivity_units, 3),
                      round(connectivity_actions, 3))

        return(out)
      }
      else{
        return(NA)
      }
    }
  }
  else if(inherits(x, "Portfolio")){

    return_list <- NULL

    for(it in seq_len(length(x$data))){

      if(it == 1){
        out <- getConnectivityPenalty(x$data[[it]])
        if(is.null(out)){
          break
        }
      }
      else{

        statusCode <- x$data[[it]]$data$status
        solution_units <- x$data[[it]]$data$sol_monitoring
        solution_actions <- x$data[[it]]$data$sol_actions

        if(!(statusCode %in% c(1,3))){

          connectivity_units <- rcpp_stats_connectivity_units(x$data[[it]]$OptimizationClass$ConservationClass$data$pu,
                                                              x$data[[it]]$OptimizationClass$ConservationClass$data$boundary,
                                                              x$data[[it]]$OptimizationClass$ConservationClass$data$dist_threats,
                                                              x$data[[it]]$OptimizationClass$ConservationClass$data$dist_features,
                                                              solution_units)

          connectivity_actions <- rcpp_stats_connectivity_actions(x$data[[it]]$OptimizationClass$ConservationClass$data$pu,
                                                          x$data[[it]]$OptimizationClass$ConservationClass$data$threats,
                                                          x$data[[it]]$OptimizationClass$ConservationClass$data$dist_threats,
                                                          x$data[[it]]$OptimizationClass$ConservationClass$data$boundary,
                                                          solution_actions)

          out[it, ] <- c(x$data[[it]]$name,
                         round(connectivity_units, 3),
                         round(connectivity_actions, 3))
        }
      }
    }
    return(out)
  }
}
