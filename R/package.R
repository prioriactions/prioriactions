## usethis namespace: start
#' @useDynLib prioriactions, .registration = TRUE
## usethis namespace: end
NULL

#' @title Create and solve multi-actions planning problems
#'
#' @description Create and solve a multi-actions planning problem. It can be used
#' instead of following the sequence of the `inputData()`, `problem()`
#' and `solve()` functions.
#'
#' @param ... arguments inherited from `inputData()`, `problem()` and `solve()` functions.

#' @name prioriactions
#'
#' @return An object of class [solution-class].
#'
#' @examples
#' \donttest{
#' ## This example uses input files included into package.
#'
#' ## set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create data instance
#' s <- prioriactions(pu = sim_pu_data, features = sim_features_data,
#'                 dist_features = sim_dist_features_data,
#'                 threats = sim_threats_data,
#'                 dist_threats = sim_dist_threats_data,
#'                 sensitivity = sim_sensitivity_data,
#'                 boundary = sim_boundary_data,
#'                 model_type = "minimizeCosts",
#'                 time_limit = 50,
#'                 output_file = FALSE,
#'                 cores = 2)
#'
#' print(s)
#' }
#' @rdname prioriactions
#' @export
prioriactions <- function(...) {

  params = list(...)

  params_data <- c(names(formals(inputData)), "sensitivity", "boundary")
  params_model <- names(formals(problem))
  params_solve <- names(formals(solve))

  #verifying input parameters
  if(!all(names(params) %in% c(params_data, params_solve, params_model))){
    id_error <- which(!names(params) %in% c(params_solve, params_model))

    stop(paste0("The following params are not defined in this function: ", paste(names(params)[id_error], collapse = " ")))
  }

  #Creating and solving mathematical model--------------------------------------------------

  conservation_model <- do.call(inputData, args = params[names(params) %in% params_data])
  #conservation_model <- inputData(params[names(params) %in% params_model])

  optimization_model <- do.call(problem, args = append(x = conservation_model,
                                                          params[names(params) %in% params_model]))

  solution <- do.call(solve, args = append(x = optimization_model,
                                           params[names(params) %in% params_solve]))

  solution
}
