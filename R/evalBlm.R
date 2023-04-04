#' @include presolve.R internal.R optimizationProblem-class.R writeOutputs.R
#' @import Matrix Rcpp
NULL

#' @title Evaluate multiple blm values
#'
#' @description Return one solution per instance for different values of blm. Like
#' `prioriactions()` function, it inherits all arguments from `inputData()`,
#' `problem()` and `solve()`.
#'
#' @param values `numeric`. Values of blm to verify. More than one value is needed.
#'
#' @param ... arguments inherited from `inputData()`, `problem()`
#'   and `solve()` functions.

#' @name evalBlm
#'
#' @return An object of class [portfolio-class].
#'
#' @details `evalblm()` creates and solves multiple instances, of the corresponding
#' multi-actions planning problem, for different values of blm. Alternatively, this
#' could be obtained by executing function `prioriactions()` or by steps the `inputData()`,
#' `problem()` and `solve()` functions; using, in each run, different blm values.
#' However, the `evalblm()` function has two advantages with
#' respect to this manual approach: : 1)
#' it is more efficient to create the models (this is because the model is created
#' just once and, at each iteration, only the blm values are updated); and 2) the
#' output is a portfolio object, which allows
#' obtaining information about the group of solutions (including all *get* functions).
#'
#' @examples
#' \donttest{
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Create model and solve
#' port <- evalBlm(pu = sim_pu_data, features = sim_features_data,
#'                 dist_features = sim_dist_features_data,
#'                 threats = sim_threats_data,
#'                 dist_threats = sim_dist_threats_data,
#'                 sensitivity = sim_sensitivity_data,
#'                 boundary = sim_boundary_data,
#'                 values = c(0.0, 0.01, 0.02, 0.03),
#'                 model_type = "minimizeCosts",
#'                 time_limit = 50,
#'                 output_file = FALSE,
#'                 cores = 2)
#'
#' getConnectivityPenalty(port)
#' }
#'
#' @rdname evalBlm
#' @export
evalBlm <- function(values = c(), ...) {

  params = list(...)

  #verifying blm length
  assertthat::assert_that(
    is.numeric(values),
    length(values) > 1
  )

  params_data <- c(names(formals(inputData)), "sensitivity", "boundary")
  params_model <- names(formals(problem))
  params_solve <- names(formals(solve))

  #verifying input parameters
  if(!all(names(params) %in% c(params_data, params_solve, params_model))){
    id_error <- which(!names(params) %in% c(params_data, params_solve, params_model))

    stop(paste0("The following params are not defined in this function: ", paste(names(params)[id_error], collapse = " ")))
  }

  #running
  repl <- length(values)
  it = 1
  name_iter = ""
  conservation_model <- do.call(inputData, args = params[names(params) %in% params_data])

  for(blm in values){

    name_iter <- paste0("Blm", blm)
    params_iter <- params
    params_iter$blm <- blm

    message(paste0(
      "*********************************",
      "\n Iteration ",it," of ",repl,": ", name_iter),
      "\n*********************************"
    )

    #Creating mathematical model--------------------------------------------------
    if(it == 1 || blm == 0){
      optimization_model <- do.call(problem, args = append(x = conservation_model,
                                                           params_iter[names(params_iter) %in% params_model]))
    }
    else if(values[it - 1] == 0 && blm != 0){
      optimization_model <- do.call(problem, args = append(x = conservation_model,
                                                           params_iter[names(params_iter) %in% params_model]))
    }
    else{
      optimization_model$data$args$blm <- blm
      optimization_model <- problem_modifier(optimization_model, blm = blm)
    }

    #changing name of output file
    if(any(names(params) %in% "name_output_file")){
      params_iter$name_output_file <- paste0(params$name_output_file, "_", name_iter)
    }
    else{
      params_iter$name_output_file <- paste0("output", "_", name_iter)
    }

    solution <- do.call(solve, args = append(x = optimization_model,
                                             params_iter[names(params_iter) %in% params_solve]))

    solution$name <- name_iter

    if(it == 1){
      portfolio <- pproto(NULL, Portfolio, data = list(solution))
    }
    else{
      portfolio$data[[it]] <- solution
    }
    it = it + 1
  }

  #exporting type of object
  portfolio
}
