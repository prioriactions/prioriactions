#' @title Extract general information about mathematical model
#'
#' @description Provides general information about the mathematical model.
#'
#' @param x [optimizationProblem-class], [solution-class] or [portfolio-class] object.
#'
#' @details `getModelInfo()` function returns five specific fields:
#' 1) **solution_name**: indicates the name of the solution, by default is *sol*.
#' 2) **model_sense**: returns the optimization sense (i.e., it indicates whether the
#' objective function is minimized or maximize).
#' 3) **n_constraints**: returns the number of constraints in the corresponding
#' mathematical optimization model.
#' 4) **n_variables**: returns the number of variables in the corresponding
#' mathematical optimization model.
#' 5) **size**: returns the size of the constraints' coefficients matrix A
#' number of constraints and number of variables).
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
#' problem_data <- inputData(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create optimization model
#' problem_model <- problem(x = problem_data, blm = 1)
#'
#' # get model information
#' getModelInfo(problem_model)
#'
#' @name getModelInfo
NULL

#' @rdname getModelInfo
#' @export
getModelInfo <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("OptimizationProblem", "Solution", "Portfolio")))

  if(inherits(x, "OptimizationProblem")){
    size_A <- utils::object.size(x$data$A)

    out <- data.frame(model_sense = ifelse(x$data$modelsense == "min","minimization", "maximization"),
                      n_constraints = base::nrow(x$data$A),
                      n_variables = base::ncol(x$data$A),
                      size = format(size_A, units = "KB", digits = 3L, standard = "SI"))
  }
  else if(inherits(x, "Solution")){
    size_A <- utils::object.size(x$OptimizationClass$data$A)

    out <- data.frame(solution_name = x$name,
                      model_sense = ifelse(x$OptimizationClass$data$modelsense == "min","minimization", "maximization"),
                      n_constraints = base::nrow(x$OptimizationClass$data$A),
                      n_variables = base::ncol(x$OptimizationClass$data$A),
                      size = format(size_A, units = "KB", digits = 3L, standard = "SI"))
  }
  else if(inherits(x, "Portfolio")){

    for(it in seq_len(length(x$data))){

      if(it == 1){
        out <- getModelInfo(x$data[[it]])
      }
      else{
        size_A <- utils::object.size(x$data[[it]]$OptimizationClass$data$A)

        out[it, ] <- c(solution_name = x$data[[it]]$name,
                       model_sense = ifelse(x$data[[it]]$OptimizationClass$data$modelsense == "min","minimization", "maximization"),
                       n_constraints = base::nrow(x$data[[it]]$OptimizationClass$data$A),
                       n_variables = base::ncol(x$data[[it]]$OptimizationClass$data$A),
                       size = format(size_A, units = "KB", digits = 3L, standard = "SI"))
      }
    }
  }
  return(out)
}
