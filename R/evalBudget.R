#' @include presolve.R internal.R optimizationProblem-class.R writeOutputs.R
#' @import Matrix Rcpp
NULL

#' @title evalBudget
#'
#' @description Create an optimization model for the multi-action conservation
#'   planning problem, following the mathematical formulations used in
#'   Salgado-Rojas *et al.* (2020). This function is used to specify model
#'   configuration parameters related to connectivity issues and its internal
#'   *modus operandi*.
#'
#'   Connectivity parameters (`blm` and `blm_actions`) manipulate the
#'   spatial fragmentation of planning units and/or action management to improve
#'   the compactness of the reserve solutions. Likewise, the other parameters
#'   (`curve` and `segments`) affect the linearization strategy
#'   used by the model internally in order to be solved under a linear
#'   programming approach. **It is not recommended to modify the default
#'   values of the later ones**.
#'
#' @param x Object of class [ConservationProblem-class()] that
#'   specifies the basic data used in a problem of prioritization of multiple
#'   conservation actions. This object must be created using the
#'   [problem()] function.
#'
#' @param blm A `numeric` value that indicates the penalty factor
#'   associated to the spatial fragmentation of planning units, similar to
#'   **Boundary Length Modifier (BLM)** in *Marxan*. This argument
#'   only has an effect when the [bound()] argument of
#'   [problem()] function is a `data.frame` object. **The
#'   default argument is zero**.
#'
#' @param blm_actions A `numeric` value that indicates the penalty factor
#'   associated to the spatial fragmentation of actions. **The default
#'   argument is zero**.
#'
#' @param curve An `integer` value that selects the type of continuous
#'   curve that will represent the expression (linear or non-linear) associated
#'   with a specific constraint in this model. Therefore, the curve can
#'   represent a linear (1), quadratic (2) or cubic (3) function. **The
#'   default argument is 3 and it is not recommended to change this value unless
#'   you have advanced knowledge of the linearization of mathematical model**.
#'
#' @param segments An `integer` value that selects the number of
#'   segments (1, 2 or 3) that will have the *piecewise linear function*
#'   in charge of approximating the non-linear expression contained in a
#'   specific constraint of this model. **The default argument is 3 and it
#'   is not recommended to change this value unless you have advanced knowledge
#'   of the linearization of mathematical model**.

#' @name evalBudget
#'
#' @return An object of class [OptimizationProblem-class()].
#'
#' @details **Put details here! The details may include the mathematical
#'  formulation of the optimization model associated with this conservation
#'  problem and/or a rough description of the mathematical model, and/or what
#'  happens when the parameters are set in one way or another.**
#'
#' @seealso For more information regarding the arguments `blm` and
#'  `blm_actions`, see the [official
#'  *Marxan* website](https://marxansolutions.org) and the article by Salgado-Rojas *et al.*
#'  (2020), respectively. Also, for more information regarding the arguments
#'  `curve` and `segments`, see the supplementary material
#'  associated with the article by Salgado-Rojas *et al.* (2020), which can
#'  be found online at <https://doi.org/10.1016/j.ecolmodel.2019.108901>.
#'
#' @examples
#' ## Create an optimization model for the multi-action conservation
#' ## planning problem using a data instance that has been created in R.
#' ## This example uses input files included into package.
#'
#' ## Load package
#' library(prioriactions)
#'
#' ## Load data
#' data(example_pu_data, example_features_data, example_dist_features_data, example_dist_threats_data, example_threats_data, example_sensitivity_data, example_bound_data)
#'
#' ## Create data instance
#' input <- list(
#'   pu = example_pu_data, features = example_features_data, dist_features = example_dist_features_data,
#'   threats = example_threats_data, dist_threats = example_dist_threats_data, sensitivity = example_sensitivity_data,
#'   bound = example_bound_data
#' )
#'
#' ## Create optimization model
#' portfolio <- evalBudget(data = input, budget = c(10, 20), output_file = FALSE, time_limit = 10)
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
