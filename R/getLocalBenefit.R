#' @title Get local benefits
#'
#' @description Provides the benefit of all the features in each planning unit in a solution.
#'
#' @param x `Solution-class` or `Portfolio-class` object.
#'
#' @details In the case of working with the presence/absence of threats (binary intensities),
#' we defined the benefit as a measure of the number of actions taken against the threats that
#' affect said feature with respect to all possible actions to do. For more information on its calculation, see the
#' [getBenefit](https://prioriactions.github.io/prioriactions/reference/getBenefit.html)
#' reference.
#'
#' The benefit can be expressed
#' mathematically for a set of planning units
#'\eqn{I} indexed by \eqn{i}, and
#'a set of threats \eqn{K} indexed by \eqn{k} as:
#'
#' \deqn{
#' \sum_{i \in I_s} \frac{ \sum_{k \in K_i \cap K_s}{x_{ik}}}{|K_i \cap K_s|}
#' }
#'
#' Here, \eqn{x_{ik}} is the decisions variable that specify whether action to abate the
#' threat \eqn{k} in the planning unit \eqn{i} has been selected (1) or not (0).
#'
#' @return [data.frame].
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
#' # get local benefit of solution
#' local_benefit <- getLocalBenefit(s)
#' head(local_benefit)
#'
#' @name getLocalBenefit
NULL

#' @rdname getLocalBenefit
#' @export
getLocalBenefit <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  if(inherits(x, "Solution")){
    return(x$data$local_benefits)
  }
  else if(inherits(x, "Portfolio")){
    cont_aux = 0

    for(it in seq_len(length(x$data))){

      if(cont_aux == 0){
        benefit_solution <- getLocalBenefit(x$data[[it]])
        colnames(benefit_solution)[3] <- x$data[[it]]$name
        cont_aux = 1
      }
      else{
        aux <- getLocalBenefit(x$data[[it]])
        benefit_solution[, it + 2] <- aux[,3]
        colnames(benefit_solution)[it + 2] <- x$data[[it]]$name
      }
    }
    return(benefit_solution)
  }
}
