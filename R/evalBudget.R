#' @include presolve.R internal.R optimizationProblem-class.R writeOutputs.R
#' @import Matrix Rcpp
NULL

#' @title Evaluate multiple budget values
#'
#' @description Provides multiple solutions for different values of budgets. This function
#' assumes that you are working with the *maximizeBenefits* model. Like
#' `prioriactions()` function, It inherits all arguments from `problem()`,
#' `maximizeBenefits()` and `solve()`.
#'
#' @param data `list`. Input data list for `problem()` function.
#'
#' @param budget `numeric`. Values of budget to verify. more than one value is
#' needed.
#'
#' @param ... arguments inherited from `problem()`, `maximizeBenefits()`,
#'   and `solve()` functions.

#' @name evalBudget
#'
#' @return An object of class [portfolio-class].
#'
#' @details `evalBudget()` creates and solves multiple multi-actions planning
#' problems for different values of budgets. You can do this by manually running
#' `prioriactions()` function with these different budget values (i.e., running
#' once by budget). However, the `evalBudget()` function has two advantages
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
#' port <- evalBudget(data = inputs, budget = c(1, 10, 50, 100), time_limit = 50, output_file = FALSE)
#'
#' plot(port)
#'
#' @rdname evalBudget
#' @export
evalBudget <- function(data = list(), ...) {

  # assert that arguments are valid
  assertthat::assert_that(
    is.list(data)
  )
  params = list(...)

  #first step: problem
  conservation_model <- do.call(problem, args = data)

  #verifying budget length
  if(any(names(params) %in% "budget")){
    assertthat::assert_that(
      is.numeric(params$budget),
      length(params$budget) > 1
    )
  }
  else{
    stop("budget param not defined")
  }


  params_solve <- c("solver", "gap_limit", "time_limit", "solution_limit", "cores",
                    "verbose", "name_output_file", "output_file")
  params_model <- c("blm", "curve", "segments", "recovery", "budget")

  name_model <- "maximizeBenefits"

  #number of replications
  repl <- length(params$budget)

  #verifying input parameters
  if(!all(names(params) %in% c(params_solve, params_model))){
    id_error <- which(!names(params) %in% c(params_solve, params_model))

    stop(paste0("The following params are not defined in this function: ", paste(names(params)[id_error], collapse = " ")))
  }


  #running
  it = 1
  name_iter = ""

  for(budget in params$budget){

    name_iter <- paste0("Budget", budget)
    params_iter <- params
    params_iter$budget <- budget

    message(paste0(
      "*********************************",
      "\n Iteration ",it," of ",repl,": ", name_iter),
      "\n*********************************"
    )

    #Creating mathematical model--------------------------------------------------
    if(it == 1){
      optimization_model <- do.call(name_model, args = append(x = conservation_model,
                                                              params_iter[names(params_iter) %in% params_model]))
    }
    else{
      rhs_size <- length(optimization_model$data$rhs)
      optimization_model$data$rhs[rhs_size] <- budget
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
