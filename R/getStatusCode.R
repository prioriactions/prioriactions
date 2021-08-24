#' @title Get status code
#'
#' @description Provides the status code of solver at the end of the optimization period.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' @return [numeric].
#'
#' @details `GetStatusCode`can have five states:
#'
#' 1) `0`: When the resolution of the model stop
#' when the quality of the solution (*gap*) is less than or equal to
#' `gap_limit` (parameter of the `solve` function).
#' 2) `1`: When the model is infeasible.
#' 3) `2`: When the resolution of the model
#' stops when a `time_limit` has been reached finding a feasible solution
#' (parameter of the `solve` function).
#' 4) `3`: When the resolution of the model
#' stops when a `time_limit` has been reached without finding a feasible solution
#' (parameter of the `solve` function).
#' 5) `4`: When the resolution of the model stops when it has
#' found the first feasible solution (`solution_limit = TRUE` parameter in `solve` function).
#' 6) `5`: For any other case.
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
#' ## get status code of solution
#' getStatusCode(s)
#'
#' @name getStatusCode
NULL

#' @rdname getStatusCode
#' @export
getStatusCode <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  if(inherits(x, "Solution")){
    return(x$data$status)
  }
  else if(inherits(x, "Portfolio")){

    return_list <- NULL

    for(it in seq_len(length(x$data))){

      out <- getStatusCode(x$data[[it]])

      return_list <- c(return_list, out)
    }

    return(return_list)
  }
}
