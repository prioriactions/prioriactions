#' @title Extract general information about solution
#'
#' @description Provides general information about the process of solving.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' @details `getPerformance()` function returns five specific fields:
#' 1) **solution_name**: indicates the name of the solution, by default is *sol*.
#' 2) **objective_value**: indicates the value of the objective function of a given solution.
#' This value depends on the type of model solved (more information in the `problem()` function).
#' 3) **gap**: returns the relative MIP optimality gap of a solution. It is measured as
#' the ratio between the objective function induced by the best known (primal solution)
#' integer solution and the objective function induced by the best node in the search
#' tree (dual solution).
#' 4) **solving_time**: indicates the solving time of mathematical model.
#' 5) **status**: provides the status of solver at the end of the optimization period.
#' This can have six states:
#' - *Optimal solution (according to gap tolerance)* : When the resolution of the model stop when
#' the quality of the solution (gap) is less than or equal to gap_limit (parameter of the `solve()` function).
#' - *No solution (model was proven to be infeasible or unbounded)*: When the model is infeasible.
#' - *Feasible solution (according to time limit)*: When the resolution of the model stops when a
#' time_limit has been reached finding a feasible solution (parameter of the `solve()` function).
#' - *No solution (according to time limit)*: When the resolution of the model stops when a time_limit
#' has been reached without finding a feasible solution (parameter of the `solve()` function).
#' - *First feasible solution*: When the resolution of the model stops when it has found the first
#' feasible solution (solution_limit = TRUE parameter in `solve()` function).
#' - *No solution information is available*: For any other case.
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
#' problem_model <- problem(x = problem_data, blm = 1)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, time_limit = 2, output_file = FALSE, cores = 2)
#'
#' # get solution gap
#' getPerformance(s)
#' }
#'
#' @name getPerformance
NULL

#' @rdname getPerformance
#' @export
getPerformance <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  if(inherits(x, "Solution")){
    if (x$data$gap == "No reported" || is.null(x$data$gap)) {
      return(NA)
    }
    else {
      out <- data.frame(solution_name = x$name,
                        objective_value = round(x$data$objval, 3),
                        gap = ifelse(is.numeric(x$data$gap),
                                     base::round(x$data$gap * 100, 3),
                                     x$data$gap),
                        solving_time = base::round(x$data$runtime, 3),
                        status = getStatus(x))
    }
  }
  else if(inherits(x, "Portfolio")){

    for(it in seq_len(length(x$data))){

      if(it == 1){
        out <- getPerformance(x$data[[it]])
      }
      else{
        out[it, ] <- c(solution_name = x$data[[it]]$name,
                       objective_value = round(x$data[[it]]$data$objval, 3),
                       gap = ifelse(is.numeric(x$data[[it]]$data$gap),
                                    base::round(x$data[[it]]$data$gap * 100, 3),
                                    x$data[[it]]$data$gap),
                       solving_time = round(x$data[[it]]$data$runtime, 3),
                       status = getStatus(x$data[[it]]))
      }
    }
  }
  return(out)
}
