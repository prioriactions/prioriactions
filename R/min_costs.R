#' @include presolve.R MAMP_model.R
#' @import Matrix
NULL

#' @title min_costs
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

#' @name min_costs
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
#' data(example_pu_data, example_features_data, example_rij_data, example_threats_data, example_sensitivity_data, example_bound_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = example_pu_data, features = example_features_data, rij = example_rij_data,
#'   threats = example_threats_data, sensitivity = example_sensibility_data,
#'   bound = example_bound_data
#' )
#'
#' ## Create optimization model
#' problem_model <- min_costs(x = problem_data, blm = 1, blm_actions = 1)
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
min_costs <- function(x, ...) UseMethod("min_costs")

#' @rdname min_costs
#' @method min_costs default
#' @export
min_costs.default <- function(x, ...) {
  stop("argument to x is not valid, it should be a Conservation problem object.")
}

#' @rdname min_costs
#' @method min_costs ConservationProblem
#' @export
min_costs.ConservationProblem <- function(x, blm = 0, blm_actions = 0, curve = 3, segments = 3, ...) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    no_extra_arguments(...)
  )

  ##bound
  assertthat::assert_that(assertthat::is.scalar(blm), is.finite(blm))
  if (abs(blm) <= 1e-10 && !is.null(x$getData("bound"))) {
    warning("The blm argument was set to 0, so the boundary data has no effect",call.=FALSE)
  }

  assertthat::assert_that(assertthat::is.scalar(blm_actions), is.finite(blm_actions))
  if (abs(blm_actions) <= 1e-10 && !is.null(x$getData("bound"))) {
    warning("The blm_actions argument was set to 0, so the boundary data has no effect",call.=FALSE)
  }

  ## blm
  assertthat::assert_that(assertthat::is.scalar(blm), is.finite(blm))
  if (abs(blm) > 1e-50 && is.null(x$getData("bound"))) {
    warning("No boundary data supplied so the blm argument has no effect",call.=FALSE)
  }

  ## blm_actions
  assertthat::assert_that(assertthat::is.scalar(blm_actions), is.finite(blm_actions))
  if (abs(blm_actions) > 1e-50 && is.null(x$getData("bound"))) {
    warning("No boundary data supplied so the blm_actions argument has no effect",call.=FALSE)
  }

  ## curve
  assertthat::assert_that(assertthat::is.scalar(curve), is.finite(curve))
  if (!curve %in% c(1, 2, 3)) {
    stop("invalid curve type")
  }

  ## segments
  assertthat::assert_that(assertthat::is.scalar(segments), is.finite(segments))
  if (!segments %in% c(1, 2, 3)) {
    stop("invalid number of segments for linearization")
  }

  ## Targets
  features <- x$getData("features")
  assertthat::has_name(features, "target")
  assertthat::assert_that(
    is.numeric(features$target),
    assertthat::noNA(features$target)
  )


  ## Presolve
  presolve(x, objective = "min costs", curve)

  ## Creating the MAMP model
  pu <- x$getData("pu")
  rij <- x$getData("rij")
  threats <- x$getData("threats")
  sensitivity <- x$getData("sensitivity")
  bound <- x$getData("bound")

  pu <- pu[, c("internal_id", "cost", "status")]
  features <- features[, c("internal_id", "target")]
  rij <- rij[, c("internal_pu", "internal_species", "amount")]
  threats <- threats[, c("internal_pu", "threats", "amount", "cost", "status")]
  sensitivity <- sensitivity[, c("internal_species", "threats", "amount")]

  if (!is.null(bound) && (abs(blm) > 1e-50 || abs(blm_actions) > 1e-50)) {
    bound <- bound[, c("internal_id1", "internal_id2", "boundary")]
  }
  else{
    bound <- NULL
  }

  if(curve == 1){
    segments = 1
  }
  settings_Data <- list(beta1 = blm, beta2 = blm_actions, exponent = curve, segments = segments)

  # MAMP_model(features, pu, bound, rij, threats, sensitivity, settings_Data, x)

  problemData <- methods::new(OptimizationProblemRcpp)

  rcpp_test <- problemData$Create_new_optimization_problem(features, pu, bound, rij, threats, sensitivity, settings_Data)

  rcpp_test$A <- Matrix::sparseMatrix(i = rcpp_test$A_i + 1, j = rcpp_test$A_j + 1, x = rcpp_test$A_x)

  # create OptimizationProblem object
  pproto(NULL, OptimizationProblem,
    data = list(
      obj = rcpp_test$C, rhs = rcpp_test$Rhs, sense = rcpp_test$Sense, vtype = rcpp_test$Type,
      A = rcpp_test$A, bounds = rcpp_test$Bounds, modelsense = rcpp_test$Modelsense,
      statistics = rcpp_test$Statistics, arg = settings_Data
    ),
    ConservationClass = x
  )
}
