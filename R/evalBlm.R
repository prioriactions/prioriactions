#' @include presolve.R internal.R optimizationProblem-class.R writeOutputs.R
#' @import Matrix Rcpp
NULL

#' @title Evaluate multiple blm values
#'
#' @description Provides multiple solutions for different values of blm. Like
#' `prioriactions()` function, It inherits all arguments from `problem()`,
#' `minimizeCosts()`, `maximizeBenefits()` and `solve()`.
#'
#' @param data `list`. Input data list for `problem()` function.
#'
#' @param name_model [character]. Name of the type of model to create. With two possible values:
#' `"minimizeCosts"` and `"maximizeBenefits"`.
#'
#' @param blm `numeric`. Values of blm to verify. More than one value is needed.
#'
#' @param ... arguments inherited from `problem()`, `minimizeCosts()`, `maximizeBenefits()`,
#'   and `solve()` functions.

#' @name evalBlm
#'
#' @return An object of class [portfolio-class].
#'
#' @details `evalblm()` creates and solves multiple multi-actions planning
#' problems for different values of blm. You can do this by manually running
#' `prioriactions()` function with these different blm values (i.e., running
#' once by blm). However, the `evalblm()` function has two advantages
#' over their counterpart: 1) it is more efficient to create the models.
#' This is because the model is once created and then updated with the
#' new information; 2) the output is a portfolio object, which allows
#' obtaining information about the group of solutions, including, all *get*
#' functions and also different types of plots.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' inputs <- list(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create model and solve
#' port <- evalBlm(data = inputs, blm = c(0.0, 0.5, 4, 8),
#'                 name_model = "minimizeCosts", time_limit = 50, output_file = FALSE)
#'
#' plot(port)
#'
#' @rdname evalBlm
#' @export
evalBlm <- function(data = list(), name_model = "minimizeCosts", ...) {

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

  #verifying blm length
  if(any(names(params) %in% "blm")){
    assertthat::assert_that(
      is.numeric(params$blm),
      length(params$blm) > 1
    )
  }
  else{
    stop("blm param not defined")
  }


  params_solve <- c("solver", "gap_limit", "time_limit", "solution_limit", "cores",
                    "verbose", "name_output_file", "output_file")
  params_model <- c("blm", "curve", "segments", "recovery")

  #number of replications
  repl <- length(params$blm)

  if(name_model == "maximizeBenefits"){
    params_model <- c(params_model, "budget")
  }

  #verifying input parameters
  if(!all(names(params) %in% c(params_solve, params_model))){
    id_error <- which(!names(params) %in% c(params_solve, params_model))

    stop(paste0("The following params are not defined in this function: ", paste(names(params)[id_error], collapse = " ")))
  }


  #running
  it = 1
  name_iter = ""

  for(blm in params$blm){

    name_iter <- paste0("Blm", blm)
    params_iter <- params
    params_iter$blm <- blm

    message(paste0(
      "*********************************",
      "\n Iteration ",it," of ",repl,": ", name_iter),
      "\n*********************************"
    )

    #Creating mathematical model--------------------------------------------------
    optimization_model <- do.call(name_model, args = append(x = conservation_model,
                                                            params_iter[names(params_iter) %in% params_model]))

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
