#' @include presolve.R internal.R optimizationProblem-class.R writeOutputs.R
#' @import Matrix Rcpp
NULL

#' @title Evaluate multiple budget values
#'
#' @description Provides multiple solutions for different values of budgets. This
#' function assumes that the *maximizeBenefits* option is being used (note that
#' the *minimizeCosts* option does not require setting a maximum budget). Like
#' `prioriactions()` function, it inherits all arguments from `problem()`,
#' `model()` and `solve()`.
#'
#' @param data `list`. Input data list for `problem()` function.
#'
#' @param values `numeric`. Values of budget to verify. More than one value is
#' needed.
#'
#' @param ... arguments inherited from `problem()`, `maximizeBenefits()`,
#'   and `solve()` functions.
#'
#' @name evalBudget
#'
#' @return An object of class [portfolio-class].
#'
#' @details `evalBudget()` creates and solves multiple instances, of the corresponding
#' multi-actions planning problem, for different values of maximum budgets. Alternatively, this
#' could be obtained by executing function `prioriactions()` or by steps the `problem()`,
#' `model()` and `solve()` functions; using, in each run, different budgtes values.
#' However, the `evalBudget()` function has two advantages with
#' respect to this manual approach: : 1)
#' it is more efficient to create the models (this is because the model is created
#' just once and, at each iteration, only the budget values are updated); and 2) the
#' output is a portfolio object, which allows
#' obtaining information about the group of solutions (including all *get* functions).
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
#' port <- evalBudget(data = inputs, values = c(1, 10, 50, 100), time_limit = 50, output_file = FALSE)
#'
#' @rdname evalBudget
#' @export
evalBudget <- function(data = list(), values = c(), ...) {

  # assert that arguments are valid
  assertthat::assert_that(
    is.list(data)
  )
  params = list(...)

  #first step: problem
  conservation_model <- do.call(problem, args = data)

  #verifying budget length
  assertthat::assert_that(
    is.numeric(values),
    length(values) > 1
  )

  params_solve <- c("solver", "gap_limit", "time_limit", "solution_limit", "cores",
                    "verbose", "name_output_file", "output_file")
  params_model <- c("blm", "curve", "segments", "recovery", "budget")

  name_model <- "maximizeBenefits"

  #number of replications
  repl <- length(values)

  #verifying input parameters
  if(!all(names(params) %in% c(params_solve, params_model))){
    id_error <- which(!names(params) %in% c(params_solve, params_model))

    stop(paste0("The following params are not defined in this function: ", paste(names(params)[id_error], collapse = " ")))
  }

  #running
  it = 1
  name_iter = ""

  for(budget in values){

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
