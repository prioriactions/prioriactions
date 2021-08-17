#' @title Get planning units
#'
#' @description Provides the selected planning units in a solution. If is a `recovery` model,
#' then all planning unit selected have at least one action to perform on it.
#' More information about of recovery models in
#' `minimizeCosts()` and `maximizeBenefits()` functions.
#'
#' @param x [solution-class] or [portfolio-class] object.
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
#' # get planning units selected as solution
#' pu <- getPlanningUnits(s)
#' head(pu)
#'
#' @name getPlanningUnits
NULL

#' @rdname getPlanningUnits
#' @export
getPlanningUnits <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  if(inherits(x, "Solution")){
    return(x$data$sol_pu)
  }
  else if(inherits(x, "Portfolio")){
    cont_aux = 0

    for(it in seq_len(length(x$data))){

      if(cont_aux == 0){
        unit_solution <- getPlanningUnits(x$data[[it]])
        colnames(unit_solution)[2] <- x$data[[it]]$name
        cont_aux = 1
      }
      else{
        aux <- getPlanningUnits(x$data[[it]])
        unit_solution[, it + 1] <- aux[,2]
        colnames(unit_solution)[it + 1] <- x$data[[it]]$name
      }
    }
    return(unit_solution)
  }
}
