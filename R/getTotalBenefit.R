#' @title Get total benefits
#'
#' @description Provides the sum of all the benefits achieved for all the features
#'in a planning exercise.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' @return [numeric].
#'
#' @seealso `getBenefit()`.
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
#' # get total benefits of a solution
#' getTotalBenefit(s)
#'
#' @name getTotalBenefit
NULL

#' @rdname getTotalBenefit
#' @export
getTotalBenefit <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  if(inherits(x, "Solution")){
    benefit <- getBenefit(x)
    ncol_benefit <- ncol(benefit)
    return(base::colSums(benefit)[ncol_benefit])
  }
  else if(inherits(x, "Portfolio")){
    return_list <- NULL

    for(it in seq_len(length(x$data))){

      out <- getTotalBenefit(x$data[[it]])

      return_list <- c(return_list, out)
    }

    return(return_list)
  }
}
