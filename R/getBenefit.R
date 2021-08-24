#' @title Get benefit
#'
#' @description Provides the total achieved benefit or maximum benefits to achieve
#' for each feature in a planning exercise. This is the sum of local benefits multiplied
#' by the amount of features in each planning unit.
#'
#' @param x `Solution-class`, `Portfolio-class` or `ConservationProblem-class` object.
#'
#' @details In the case of working with the presence/absence of threats (binary intensities),
#' we defined the benefit as a measure of the number of actions taken against the threats that
#' affect said feature with respect to all possible actions to do. This value is multiplied by
#' the amount of feature in that planning unit. For more information on its calculation, see the
#' [getBenefit](https://prioriactions.github.io/prioriactions/reference/getBenefit.html)
#' reference.
#'
#' The benefit can be expressed
#' mathematically for a set of planning units
#'\eqn{I} indexed by \eqn{i}, and
#'a set of threats \eqn{K} indexed by \eqn{k} as:
#'
#' \deqn{
#' \sum_{i \in I_s} \frac{ \sum_{k \in K_i \cap K_s}{x_{ik}}}{|K_i \cap K_s|} r_{is}
#' }
#'
#' Here, \eqn{x_{ik}} is the decisions variable that specify whether action to abate the
#' threat \eqn{k} in the planning unit \eqn{i} has been selected (1) or not (0), \eqn{r_{is}}
#' is the amount of feature \eqn{s} in planning unit \eqn{i}.
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
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Get maximum benefits to obtain
#' getBenefit(problem_data)
#'
#' ## Create optimization model
#' problem_model <- minimizeCosts(x = problem_data, blm = 1)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, solver = "gurobi", gap_limit = 0.01, output_file = FALSE)
#'
#' # get benefits of solution
#' getBenefit(s)
#'
#' @name getBenefit
NULL

#' @rdname getBenefit
#' @export
getBenefit <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "ConservationProblem", "Portfolio")))

  if(inherits(x, "Solution")){

    recovery = x$OptimizationClass$data$args$recovery

    if(!(getStatusCode(x) %in% c(1,3))){
      solution_units <- getPlanningUnits(x)
      solution_actions <- getActions(x, format = "large")

      benefits <- rcpp_stats_benefit(x$OptimizationClass$ConservationClass$data$pu,
                                     x$OptimizationClass$ConservationClass$data$features,
                                     x$OptimizationClass$ConservationClass$data$dist_features,
                                     x$OptimizationClass$ConservationClass$data$threats,
                                     x$OptimizationClass$ConservationClass$data$dist_threats,
                                     x$OptimizationClass$ConservationClass$data$sensitivity,
                                     c(solution_units$solution, solution_actions$solution),
                                     recovery)

    }
    else{
      benefits <- rcpp_stats_benefit(x$OptimizationClass$ConservationClass$data$pu,
                                     x$OptimizationClass$ConservationClass$data$features,
                                     x$OptimizationClass$ConservationClass$data$dist_features,
                                     x$OptimizationClass$ConservationClass$data$threats,
                                     x$OptimizationClass$ConservationClass$data$dist_threats,
                                     x$OptimizationClass$ConservationClass$data$sensitivity,
                                     0,
                                     TRUE)
      if(isTRUE(recovery)){
        benefits$benefit.recovery. <- NA
      }
      else{
        benefits$benefit.nothing. <- NA
        benefits$benefit.recovery. <- NA
        benefits$benefit.total. <- NA
      }
    }

    #if(x$OptimizationClass$data$args$name_model == "minimizeCosts"){
    #  benefits$target <- x$OptimizationClass$ConservationClass$data$features$target
    #}

    col_benefit <- ncol(benefits)

    if(recovery){
      colnames(benefits)[col_benefit] <- paste0(colnames(benefits)[col_benefit], x$name)
    }
    else{
      colnames(benefits)[col_benefit - 2] <- paste0(colnames(benefits)[col_benefit - 2], x$name)
      colnames(benefits)[col_benefit - 1] <- paste0(colnames(benefits)[col_benefit - 1], x$name)
      colnames(benefits)[col_benefit] <- paste0(colnames(benefits)[col_benefit], x$name)
    }


    return(benefits)
  }
  else if(inherits(x, "ConservationProblem")){

    benefits <- rcpp_stats_benefit(x$data$pu,
                                   x$data$features,
                                   x$data$dist_features,
                                   x$data$threats,
                                   x$data$dist_threats,
                                   x$data$sensitivity,
                                   0,
                                   TRUE)
    return(benefits)
  }
  else if(inherits(x, "Portfolio")){
    cont_aux = 0

    for(it in seq_len(length(x$data))){

      recovery = x$data[[it]]$OptimizationClass$data$args$recovery
      benefit_solution <- getBenefit(x$data[[it]])
      col_benefit <- ncol(benefit_solution)

      if(cont_aux == 0){
        benefit <- benefit_solution
        if(recovery){
          colnames(benefit)[col_benefit] <- paste0(colnames(benefit_solution)[col_benefit], x$data[[it]]$name)
        }
        else{
          colnames(benefit)[col_benefit - 2] <- paste0(colnames(benefit_solution)[col_benefit - 2], x$data[[it]]$name)
          colnames(benefit)[col_benefit - 1] <- paste0(colnames(benefit_solution)[col_benefit - 1], x$data[[it]]$name)
          colnames(benefit)[col_benefit] <- paste0(colnames(benefit_solution)[col_benefit], x$data[[it]]$name)
        }
        cont_aux = 1
        col_total <- col_benefit
      }
      else{
        if(recovery){
          benefit[, col_total + 1] <- benefit_solution[, col_benefit]
          colnames(benefit)[col_total + 1] <- paste0(colnames(benefit_solution)[col_benefit], x$data[[it]]$name)
          col_total <- col_total + 1
        }
        else{
          benefit[, col_total + 1] <- benefit_solution[, col_benefit - 2]
          benefit[, col_total + 2] <- benefit_solution[, col_benefit - 1]
          benefit[, col_total + 3] <- benefit_solution[, col_benefit]

          colnames(benefit)[col_total + 1] <- paste0(colnames(benefit_solution)[col_benefit - 2], x$data[[it]]$name)
          colnames(benefit)[col_total + 2] <- paste0(colnames(benefit_solution)[col_benefit - 1], x$data[[it]]$name)
          colnames(benefit)[col_total + 3] <- paste0(colnames(benefit_solution)[col_benefit], x$data[[it]]$name)

          col_total <- col_total + 3
        }
      }
    }
    return(benefit)
  }
}
