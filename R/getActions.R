#' @title Get actions
#'
#' @description Provides the selected actions in a solution.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' @param format `string`. Output format of the action matrix. `"wide"` format
#' shows one column per action, while `"large"` format shows three columns: pu,
#' threat and solution.
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
#' # get actions information in large format
#' actions <- getActions(s, format = "large")
#' head(actions)
#'
#' # get actions information in wide format
#' actions <- getActions(s, format = "wide")
#' head(actions)
#'
#' @name getActions
NULL

#' @rdname getActions
#' @export
getActions <- function(x, format = "large") {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")))

  if(inherits(x, "Solution")){

    if (format == "wide") {
      return(x$data$sol_actions_extended)
    }
    else if (format == "large") {
      return(x$data$sol_actions_reduced)
    }
    else {
      paste0("The type should be 'large' or 'wide'")
    }
  }
  else if(inherits(x, "Portfolio")){

    cont_aux = 0

    for(it in seq_len(length(x$data))){

      if(cont_aux == 0){
        action_solution <- getActions(x$data[[it]], format = "large")
        colnames(action_solution)[3] <- x$data[[it]]$name
        cont_aux = 1
      }
      else{
        aux <- getActions(x$data[[it]], format = "large")
        action_solution[, it + 2] <- aux[,3]
        colnames(action_solution)[it + 2] <- x$data[[it]]$name
      }
    }
    return(action_solution)
  }
}
