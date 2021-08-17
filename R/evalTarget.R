#' @include presolve.R internal.R optimizationProblem-class.R writeOutputs.R
#' @import Matrix Rcpp
NULL

#' @title sensitivityAnalysisBlm
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

#' @name evalTarget
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
#' portfolio <- evalTarget(data = input, prop = c(0.1, 0.2), output_file = FALSE, time_limit = 10)
#'

#' @rdname evalTarget
#' @export
evalTarget <- function(data = list(), prop = 1, ...) {

  # assert that arguments are valid
  assertthat::assert_that(
    is.list(data)
  )
  params = list(...)

  #first step: problem
  conservation_model <- do.call(problem, args = data)

  #verifying prop length
  assertthat::assert_that(
      is.numeric(prop),
      length(prop) > 1
  )

  #prop_Target
  if(any(prop < 0) || any(prop > 1)){
    stop("invalid prop param")
  }

  if(any(names(params) %in% "recovery")){
    recovery = params$recovery
  }
  else{
    recovery = TRUE
  }


  params_solve <- c("solver", "gap_limit", "time_limit", "solution_limit", "cores",
                    "verbose", "name_output_file", "output_file")
  params_model <- c("blm", "curve", "segments", "recovery")

  name_model <- "minimizeCosts"

  #number of replications
  repl <- length(prop)

  #verifying input parameters
  if(!all(names(params) %in% c(params_solve, params_model))){
    id_error <- which(!names(params) %in% c(params_solve, params_model))

    stop(paste0("The following params are not defined in this function: ", paste(names(params)[id_error], collapse = " ")))
  }


  #running
  it = 1
  name_iter = ""

  if(recovery){
    maximum_target <- getBenefit(conservation_model)$benefit.recovery
  }
  else{
    maximum_target <- getBenefit(conservation_model)$benefit.total
  }

  for(i in prop){

    name_iter <- paste0("Prop", i)
    params_iter <- params

    conservation_model$data$features$target <- maximum_target * i

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
      features_size <- conservation_model$getFeatureAmount()

      optimization_model$data$rhs[(rhs_size - features_size + 1):rhs_size] <- conservation_model$data$features$target
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