#' @title Get number of constraints
#'
#' @description Provides the number of constraints of mathematical model.
#'
#' @param x [optimizationProblem-class], [solution-class] or [portfolio-class] object.
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
#' ## get number of constraints of model
#' getNconstraints(problem_model)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, solver = "gurobi", gap_limit = 0.01, output_file = FALSE)
#'
#' ## get number of constraints of model
#' getNconstraints(s)
#'
#' @name getNconstraints
NULL

#' @rdname getNconstraints
#' @export
getNconstraints <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("OptimizationProblem", "Solution", "Portfolio")))

  if(inherits(x, "OptimizationProblem")){
    return(base::nrow(x$data$A))
  }
  else if(inherits(x, "Solution")){
    return(getNconstraints(x$OptimizationClass))
  }
  else if(inherits(x, "Portfolio")){

    return_list <- NULL

    for(it in seq_len(length(x$data))){

      out <- getNconstraints(x$data[[it]])

      return_list <- c(return_list, out)
    }

    return(return_list)
  }
}
