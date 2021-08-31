#' @title Get size of matrix A of model
#'
#' @description Returns the size of the constraints' coefficients matrix A
#' (number of constraints and number of variables). Contains all
#' the variables and constraints of the model.
#'
#' @param x [optimizationProblem-class], [solution-class] or [portfolio-class] object.
#'
#' @return [character].
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
#' ## get number of constraints of model
#' getNconstraints(problem_model)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, solver = "gurobi", gap_limit = 0.01, output_file = FALSE)
#'
#' ## get size of matrix A of model
#' getSizeA(s)
#'
#' @name getSizeA
NULL

#' @rdname getSizeA
#' @export
getSizeA <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("OptimizationProblem", "Solution", "Portfolio")))

  if(inherits(x, "OptimizationProblem")){
    size_A <- utils::object.size((x$data$A))

    return(format(size_A, units = "KB", digits = 3L, standard = "SI"))

  }
  else if(inherits(x, "Solution")){
    out <- getSizeA(x$OptimizationClass)
    return(out)
  }
  else if(inherits(x, "Portfolio")){

    return_list <- NULL

    for(it in seq_len(length(x$data))){

      out <- getSizeA(x$data[[it]])

      return_list <- c(return_list, out)
    }

    return(return_list)
  }

}
