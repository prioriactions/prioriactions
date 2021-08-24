## usethis namespace: start
#' @useDynLib prioriactions, .registration = TRUE
## usethis namespace: end
NULL

#' @title Create and solve multi-actions planning problems
#'
#' @description Create and solve a multi-actions planning problem. It can be used
#' instead of following the sequence of the `problem()`, `minimizeCosts()`/`maximizeBenefits()`
#' and `solve()` functions.
#'
#' @param data `list`. Input data list for `problem()` function.
#'
#' @param name_model [character]. Name of the type of model to create. With two possible values:
#' `"minimizeCosts"` and `"maximizeBenefits"`.
#'
#' @param ... arguments inherited from `problem()`, `minimizeCosts()`, `maximizeBenefits()`,
#'   and `solve()` functions.

#' @name prioriactions
#'
#' @return An object of class [solution-class].
#'
#' @examples
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
#' input <- problem(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create and solve optimization model
#' s <- prioriactions(data = input, name_model = "minimizeCosts",
#'                    blm = 0, output_file = FALSE, time_limit = 10)
#'
#' print(s)
#'
#' @rdname prioriactions
#' @export
prioriactions <- function(data = list(), name_model = "minimizeCosts", ...) {

  # assert that arguments are valid
  assertthat::assert_that(
    is.list(data)
  )
  params = list(...)

  #Verifying name models
  if (!name_model %in% c("minimizeCosts", "maximizeBenefits")) {
    stop("invalid name model")
  }

  #verifying boundary presence
  conservation_model <- do.call(problem, args = data)

  params_solve <- c("solver", "gap_limit", "time_limit", "solution_limit", "cores",
                    "verbose", "name_output_file", "output_file")
  params_model <- c("blm", "curve", "segments", "recovery")

  if(name_model == "maximizeBenefits"){
    params_model <- c(params_model, "budget")
  }

  #verifying input parameters
  if(!all(names(params) %in% c(params_solve, params_model))){
    id_error <- which(!names(params) %in% c(params_solve, params_model))

    stop(paste0("The following params are not defined in this function: ", paste(names(params)[id_error], collapse = " ")))
  }

  #Creating mathematical model--------------------------------------------------
  optimization_model <- do.call(name_model, args = append(x = conservation_model,
                                                          params[names(params) %in% params_model]))


  solution <- do.call(solve, args = append(x = optimization_model,
                                           params[names(params) %in% params_solve]))

  solution
}
