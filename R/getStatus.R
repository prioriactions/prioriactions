#' @title Get status
#'
#' @description Provides the status of solver at the end of the optimization period.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' @return [character].
#'
#' @details `GetStatus` can have five states:
#'
#' 1) *Optimal solution (according to gap tolerance)* : When the resolution of the model stop
#' when the quality of the solution (*gap*) is less than or equal to
#' `gap_limit` (parameter of the `solve` function).
#' 2) *No solution (model was proven to be infeasible or unbounded)*: When the model is infeasible.
#' 3) *Feasible solution (according to time limit)*: When the resolution of the model
#' stops when a `time_limit` has been reached finding a feasible solution
#' (parameter of the `solve` function).
#' 4) *No solution (according to time limit)*: When the resolution of the model
#' stops when a `time_limit` has been reached without finding a feasible solution
#' (parameter of the `solve` function).
#' 5) *First feasible solution*: When the resolution of the model stops when it has
#' found the first feasible solution (`solution_limit = TRUE` parameter in `solve` function).
#' 6) *No solution information is available*: For any other case.
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
#' ## get status of solution
#' getStatus(s)
#'
#' @name getStatus
NULL

#' @rdname getStatus
#' @export
getStatus <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  if(inherits(x, "Solution")){
    statusCode <- getStatusCode(x)
    gap <- getGap(x)
    time_limit <- x$data$arg$timelimit
    if(statusCode == 0L){
      return(paste0("Optimal solution (according to gap tolerance: ", gap,")"))
    }
    else if(statusCode == 1L){
      return("No solution (model was proven to be infeasible or unbounded)")
    }
    else if(statusCode == 2L){
      return(paste0("Feasible solution (according to time limit: ", time_limit, " sec)"))
    }
    else if(statusCode == 3L){
      return(paste0("No solution (according to time limit: ", time_limit, " sec)"))
    }
    else if(statusCode == 4L){
      return("First feasible solution")
    }
    else{
      return("No solution information is available")
    }
  }
  else if(inherits(x, "Portfolio")){

    return_list <- NULL

    for(it in seq_len(length(x$data))){

      out <- getStatus(x$data[[it]])

      return_list <- c(return_list, out)
    }

    return(return_list)
  }
}
