#' @include presolve.R internal.R optimizationProblem-class.R writeOutputs.R
#' @import Matrix Rcpp
NULL

#' @title sensitivityAnalysisBlm
#'
#' @description Create an optimization model for the multi-action conservation
#'   planning problem, following the mathematical formulations used in
#'   Salgado-Rojas \emph{et al.} (2020). This function is used to specify model
#'   configuration parameters related to connectivity issues and its internal
#'   \emph{modus operandi}.
#'
#'   Connectivity parameters (\code{blm} and \code{blm_actions}) manipulate the
#'   spatial fragmentation of planning units and/or action management to improve
#'   the compactness of the reserve solutions. Likewise, the other parameters
#'   (\code{curve} and \code{segments}) affect the linearization strategy
#'   used by the model internally in order to be solved under a linear
#'   programming approach. \strong{It is not recommended to modify the default
#'   values of the later ones}.
#'
#' @param x Object of class \code{\link{ConservationProblem-class}} that
#'   specifies the basic data used in a problem of prioritization of multiple
#'   conservation actions. This object must be created using the
#'   \code{\link{problem}} function.
#'
#' @param blm A \code{numeric} value that indicates the penalty factor
#'   associated to the spatial fragmentation of planning units, similar to
#'   \strong{Boundary Length Modifier (BLM)} in \emph{Marxan}. This argument
#'   only has an effect when the \code{\link{bound}} argument of
#'   \code{\link{problem}} function is a \code{data.frame} object. \strong{The
#'   default argument is zero}.
#'
#' @param blm_actions A \code{numeric} value that indicates the penalty factor
#'   associated to the spatial fragmentation of actions. \strong{The default
#'   argument is zero}.
#'
#' @param curve An \code{integer} value that selects the type of continuous
#'   curve that will represent the expression (linear or non-linear) associated
#'   with a specific constraint in this model. Therefore, the curve can
#'   represent a linear (1), quadratic (2) or cubic (3) function. \strong{The
#'   default argument is 3 and it is not recommended to change this value unless
#'   you have advanced knowledge of the linearization of mathematical model}.
#'
#' @param segments An \code{integer} value that selects the number of
#'   segments (1, 2 or 3) that will have the \emph{piecewise linear function}
#'   in charge of approximating the non-linear expression contained in a
#'   specific constraint of this model. \strong{The default argument is 3 and it
#'   is not recommended to change this value unless you have advanced knowledge
#'   of the linearization of mathematical model}.

#' @name prioriactions
#'
#' @return An object of class \code{\link{OptimizationProblem-class}}.
#'
#' @details \strong{Put details here! The details may include the mathematical
#'  formulation of the optimization model associated with this conservation
#'  problem and/or a rough description of the mathematical model, and/or what
#'  happens when the parameters are set in one way or another.}
#'
#' @seealso For more information regarding the arguments \code{blm} and
#'  \code{blm_actions}, see the \href{https://marxansolutions.org}{official
#'  \emph{Marxan} website} and the article by Salgado-Rojas \emph{et al.}
#'  (2020), respectively. Also, for more information regarding the arguments
#'  \code{curve} and \code{segments}, see the supplementary material
#'  associated with the article by Salgado-Rojas \emph{et al.} (2020), which can
#'  be found online at \url{https://doi.org/10.1016/j.ecolmodel.2019.108901}.
#'
#' @examples
#' ## Create an optimization model for the multi-action conservation
#' ## planning problem using a data instance that has been created in R.
#' ## This example uses input files included into package.
#'
#' ## Load data
#' data(example_pu_data, example_features_data, example_dist_features_data, example_dist_threats_data, example_threats_data, example_sensitivity_data, example_bound_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = example_pu_data, features = example_features_data, dist_features = example_dist_features_data,
#'   threats = example_threats_data, dist_threats = example_dist_threats_data, sensitivity = example_sensitivity_data,
#'   bound = example_bound_data
#' )
#'
#' ## Create optimization model
#' problem_model <- prioriactions(x = problem_data, blm = 1)
#'
#' ## Model summary
#' print(problem_model)
#' @references \itemize{ \item Salgado-Rojas J, <U+00C1>lvarez-Miranda E, Hermoso V,
#'  Garcia-Gonzalo J, Weintraub A. \emph{A mixed integer programming approach
#'  for multi-action planning for threat management}. Ecological Modelling 2020;
#'  418:108901. \cr (DOI: \url{https://doi.org/10.1016/j.ecolmodel.2019.108901})
#'  }
#'
#'
#'
#' @export
prioriactions <- function(x, ...) UseMethod("prioriactions")

#' @rdname prioriactions
#' @export
prioriactions <- function(data = list(), model = "minimizeCosts", ...) {

  # assert that arguments are valid
  assertthat::assert_that(
    is.list(data)
  )
  params = list(...)

  #Verifying name models
  if (!model %in% c("minimizeCosts", "maximizeBenefits")) {
    stop("invalid name model")
  }

  #verifying boundary presence
  if(length(data) == 7){
    conservation_model <- problem(data[[1]], data[[2]], data[[3]], data[[4]], data[[5]], data[[6]],
                                  inputs[[7]])
  }
  else{
    conservation_model <- problem(data[[1]], data[[2]], data[[3]], data[[4]], data[[5]], data[[6]])
  }

  #verifying blm length
  if(any(names(params) %in% c("blm"))){
    assertthat::assert_that(
      is.numeric(params$blm)
    )
  }
  else{
    params$blm <- 0
  }

  #verifying budget length
  if(any(names(params) %in% c("budget"))){
    assertthat::assert_that(
      is.numeric(params$budget)
    )
  }
  else{
    params$budget <- 0
  }

  params_solve <- names(params) %in% c("solver", "gap_limit", "time_limit", "solution_limit", "cores",
                                       "verbose", "name_output_file", "name_log", "output_file", "log_file")


  if(model == "minimizeCosts"){

    if(any(names(params) %in% c("budget"))){
      if(any(params[["budget"]] > 0)){
        warning("budget parameter has no effect on the minimizeCosts model", call. = FALSE, immediate. = TRUE)
      }
    }

    params_model <- names(params) %in% c("blm", "curve", "segments", "recovery")

    #number of replications
    iter_size <- length(params$blm)

    for(it in 1:iter_size){

      params_iter <- params
      params_iter$blm <- params$blm[it]

      message(paste0("Iteration 1 of ",iter_size,": blm = ", params_iter$blm))

      #Creating mathematical model--------------------------------------------------
      optimization_model <- do.call(minimizeCosts, args = append(x = conservation_model, params_iter[params_model]))

      #changing name of output file
      if(any(names(params_iter) %in% c("name_output_file"))){
        params_iter$name_output_file <- paste0(params_iter$name_output_file,"_blm", params_iter$blm)
      }
      else{
        params_iter$name_output_file <- paste0("prioriaction_output_blm", params_iter$blm)
      }

      solution <- do.call(solve, args = append(x = optimization_model, params_iter[params_solve]))

      if(iter_size != 1){
        if(it == 1){
          portafolio <- pproto(NULL, Portafolio, data = list(solution))
        }
        else{
          portafolio$data[[it]] <- solution
        }
      }
    }
  }
  else if(model == "maximizeBenefits"){
    params_model <- names(params) %in% c("blm", "curve", "segments", "recovery","budget")

    #number of replications
    iter_size <- length(params$budget)

    for(it in 1:iter_size){

      params_iter <- params
      params_iter$budget <- params$budget[it]

      message(paste0("Iteration 1 of ",iter_size,": budget = ", params_iter$budget))

      #Creating mathematical model--------------------------------------------------
      optimization_model <- do.call(maximizeBenefits, args = append(x = conservation_model, params_iter[params_model]))


      #changing name of output file
      if(any(names(params_iter) %in% c("name_output_file"))){
        params_iter$name_output_file <- paste0(params_iter$name_output_file,"_budget", params_iter$budget)
      }
      else{
        params_iter$name_output_file <- paste0("prioriaction_output_budget", params_iter$budget)
      }


      solution <- do.call(solve, args = append(x = optimization_model, params_iter[params_solve]))

      if(iter_size != 1){
        if(it == 1){
          portafolio <- pproto(NULL, Portafolio, data = list(solution))
        }
        else{
          portafolio$data[[it]] <- solution
        }
      }
    }
  }

  #exporting type of object
  if(iter_size != 1){
    portafolio
  }
  else{
    solution
  }
}
