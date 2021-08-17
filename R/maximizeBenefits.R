#' @include presolve.R
#' @import Matrix Rcpp
NULL

#' @title maximizeBenefits
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

#' @name maximizeBenefits
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
#' problem_model <- maximizeBenefits(x = problem_data, budget = 10)

#' @export
maximizeBenefits <- function(x, ...) UseMethod("maximizeBenefits")

#' @rdname maximizeBenefits
#' @export
maximizeBenefits <- function(x, blm = 0, budget = 0, curve = 3, segments = 3, recovery = TRUE) {

  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    assertthat::is.flag(recovery),
    assertthat::is.scalar(blm),
    is.finite(blm),
    assertthat::is.scalar(curve),
    is.finite(curve),
    assertthat::is.scalar(segments),
    is.finite(segments),
    is.numeric(budget),
    assertthat::is.scalar(budget))

  ## Presolve
  name_model <- "maximizeBenefits"
  budget <- presolve(x, name_model = name_model, recovery, budget)

  ## Getting data
  pu <- x$getData("pu")
  features <- x$getData("features")
  dist_features <- x$getData("dist_features")
  threats <- x$getData("threats")
  dist_threats <- x$getData("dist_threats")
  sensitivity <- x$getData("sensitivity")
  boundary <- x$getData("boundary")

  pu <- pu[, c("internal_id", "cost", "status")]
  features <- features[, c("internal_id")]
  dist_features <- dist_features[, c("internal_pu", "internal_feature", "amount")]
  threats <- threats[, c("internal_id", "blm_actions")]
  dist_threats <- dist_threats[, c("internal_pu", "internal_threat", "amount", "cost", "status")]
  sensitivity <- sensitivity[, c("internal_feature", "internal_threat", "a", "b", "c", "d")]

  if (!is.null(boundary)) {
    boundary <- boundary[, c("internal_id1", "internal_id2", "boundary")]
  }

  ##blm
  if (abs(blm) <= 1e-10 && !is.null(boundary)) {
    warning("The blm argument was set to 0, so the boundary data has no effect",call.=FALSE, immediate. = TRUE)
  }

  if (abs(blm) > 1e-50 && is.null(x$getData("boundary"))) {
    warning("No boundary data supplied so the blm argument has no effect",call.=FALSE, immediate. = TRUE)
  }

  if (all(threats$blm_actions <= 1e-10) && !is.null(boundary)) {
    warning("Some blm_actions argument were set to 0, so the boundary data has no effect for these cases",call.=FALSE, immediate. = TRUE)
  }

  ## blm_actions
  assertthat::assert_that(all(is.finite(threats$blm_actions)))
  if (any(threats$blm_actions > 1e-10) && is.null(boundary)) {
    warning("No boundary data supplied so the blm_actions arguments has no effect",call.=FALSE, immediate. = TRUE)
  }

  ## curve
  if (!curve %in% c(1, 2, 3)) {
    stop("invalid curve type")
  }

  if(curve != 1 && isTRUE(recovery) && any(dist_threats$amount < 1)){
    curve = 1
    warning("Curve set to 1 because there are non-binary values in threat amount for a recovery target",call.=FALSE, immediate. = TRUE)
  }

  if(curve == 1){
    segments = 1
  }

  ## segments
  if (!segments %in% c(1, 2, 3)) {
    stop("invalid number of segments for linearization")
  }

  #Creating mathematical model--------------------------------------------------
  op <- rcpp_new_optimization_problem()
  rcpp_objective_max_coverage(op, pu, features, dist_features, threats, dist_threats, boundary, blm, curve)
  rcpp_constraint_benefit(op, pu, features, dist_features, threats, dist_threats, sensitivity, recovery)
  rcpp_constraint_activation(op, pu, threats, dist_threats)
  rcpp_constraint_lock(op, pu, dist_threats)
  rcpp_constraint_budget(op, pu, dist_threats, budget)

  #Getting model from cpp-------------------------------------------------------

  model <- rcpp_optimization_problem_as_list(op)
  args <- list(blm = blm, curve = curve,
               segments = segments, budget = budget,
               recovery = recovery,
               name_model = name_model)

  model$A <- Matrix::sparseMatrix(i = model$A_i + 1, j = model$A_j + 1, x = model$A_x)

  #create list of curve items---------------------------------------------------
  if(curve != 1){
    genconpow <- list()

    for(i in 1:length(model$xvar))
      genconpow[[i]] <- list(xvar = model$xvar[i] + 1, yvar = model$yvar[i] + 1, a = curve)
  }
  else{
    genconpow <- NULL
  }

  # create Optimization Problem object-------------------------------------------

  pproto(NULL, OptimizationProblem,
         data = list(
           obj = model$obj, rhs = model$rhs, sense = model$sense, vtype = model$vtype,
           A = model$A, bounds = model$bounds,
           modelsense = model$modelsense,
           genconpow = genconpow,
           args = args
         ),
         ConservationClass = x
  )
}
