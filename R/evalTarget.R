#' @include presolve.R internal.R optimizationProblem-class.R writeOutputs.R
#' @import Matrix Rcpp
NULL

#' @title Evaluate multiple target values
#'
#' @description Return one solution per instance for different targets values. This
#' function assumes that the *minimizeCosts* model is being used. As well as the
#' `prioriactions()` function, it inherits all arguments from `inputData()`,
#' `problem()` and `solve()`.
#'
#' @param values `numeric`. Proportion of maximum value of benefits to verify (both
#' recovery and conservation benefits). This information can be obtained with
#' `getPotentialBenefit()` function. More than one value is needed.
#'
#' @param ... arguments inherited from `inputData()`, `problem()`,
#'   and `solve()` functions.

#' @name evalTarget
#'
#' @return An object of class [portfolio-class].
#'
#' @details `evalTarget()` creates and solves multiple instances, of the corresponding
#' multi-actions planning problem, for different proportions of maximum benefit values
#' as target values. It is assumed that the same proportion is applied for the maximum
#' benefit in recovery and conservation. Alternatively, this
#' could be obtained by executing function `prioriactions()` or by steps the `inputData()`,
#' `problem()` and `solve()` functions; using, in each run, different targets values.
#' However, the `evalTarget()` function has two advantages with
#' respect to this manual approach: : 1)
#' it is more efficient to create the models (this is because the model is created
#' just once and, at each iteration, only the target values are updated); and 2) the
#' output is a portfolio object, which allows
#' obtaining information about the group of solutions (including all *get* functions).
#'
#' @examples
#' \donttest{
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Create model and solve
#' port <- evalTarget(pu = sim_pu_data, features = sim_features_data,
#'                 dist_features = sim_dist_features_data,
#'                 threats = sim_threats_data,
#'                 dist_threats = sim_dist_threats_data,
#'                 sensitivity = sim_sensitivity_data,
#'                 boundary = sim_boundary_data,
#'                 values = c(0.1, 0.3, 0.5),
#'                 time_limit = 50,
#'                 output_file = FALSE,
#'                 cores = 2)
#'
#' getCost(port)
#' }
#'
#' @rdname evalTarget
#' @export
evalTarget <- function(values = c(), ...) {

  params = list(...)

  #verifying prop length
  assertthat::assert_that(
    is.numeric(values),
    length(values) > 1
  )

  #prop_Target
  if(any(values < 0) || any(values > 1)){
    stop("invalid prop param")
  }

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
  potential_benefit <- getPotentialBenefit(conservation_model)
  maximum_target_recovery <- potential_benefit$maximum.recovery.benefit
  maximum_target_conservation <- potential_benefit$maximum.conservation.benefit

  for(i in values){

    name_iter <- paste0("Prop", base::round(i, 3))
    params_iter <- c(params, model_type = "minimizeCosts")

    message(paste0(
      "*********************************",
      "\n Iteration ",it," of ",repl,": ", name_iter),
      "\n*********************************"
    )

    #Creating mathematical model--------------------------------------------------
    if(it == 1){
      conservation_model$data$features$target_recovery <- maximum_target_recovery * i
      #conservation_model$data$features$target_conservation <- maximum_target_conservation * i

      optimization_model <- do.call(problem, args = append(x = conservation_model,
                                                              params_iter[names(params_iter) %in% params_model]))
    }
    else{
      # problem modifier
      optimization_model <- problem_modifier(optimization_model, prop_target = i)
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
