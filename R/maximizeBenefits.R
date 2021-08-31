#' @include presolve.R
#' @import Matrix Rcpp
NULL

#' @title Create mathematical model: maximize benefits
#'
#' @description Create an optimization model that maximize the sum of benefits of
#'   all features without exceeding a budget limit for the multi-action conservation
#'   planning problem. Following the mathematical formulations used in
#'   Salgado-Rojas *et al.* (2020). More information about mathematical formulation
#'   in detail section.
#'
#' @param x [conservationProblem-class] object. Data used in a problem of
#'   prioritization of multiple conservation actions. This object must be created using the
#'   [problem()] function.
#'
#' @param blm `numeric`. Penalty factor associated to the spatial fragmentation of planning
#'   units, similar to Boundary Length Modifier (BLM) in *Marxan*. This argument
#'   only has an effect when the `boundary` is available.
#'
#' @param budget `numeric`. Maximum budget to perform actions.
#'
#' @param curve `integer`. Type of continuous curve used to represent benefit expression. It can
#'   be a linear (`1`), quadratic (`2`) or cubic (`3`) function.
#'   See **Details** for more information.
#'
#' @param segments `integer`. Number of segments (`1`, `2`, or `3`) used to approximate the non-linear
#'  expression (`curve`) in the calculate benefits. See **Details** for more information.
#'
#' @param recovery `logical`. Indicates if it is a recovery (`TRUE`) or conservation problem (`FALSE`).
#' A recovery problem assumes no benefits for the features by incorporating units to the solution
#' where threats are not found (i.e. no co-occurrence between the feature and
#' its threats on that site).
#'
#' @return An object of class [optimizationProblem-class].
#'
#' @name maximizeBenefits
NULL

#' @details The maximize benefits model seeks to find the set of actions that
#' maximizes the sum of benefits of all features, while the cost of performing
#' actions does not exceed a certain budget.
#'
#'This model can be expressed mathematically for a set of planning units
#'\eqn{I} indexed by \eqn{i} a set of features \eqn{S} indexed by \eqn{s}, and
#'a set of threats \eqn{K} indexed by \eqn{k} as:
#'
#' \deqn{
#' \max \space \sum_{i \in I}\sum_{s \in S_i} b_{is} r_{is} \\
#' \mathit{s.t.} \\
#'\sum_{i \in I} \sum_{k \in K_i} x_{ik} c_{ik} \leq budget
#' }
#'
#' Here, \eqn{x_{ik}} is the decisions variable that specify whether action to abate the
#' threat \eqn{k} in the planning unit \eqn{i} has been selected (1) or not (0), \eqn{c_{ik}}
#' is the cost of do the action to abate the threat \eqn{k} in the planning unit \eqn{i},
#' \eqn{b_{is}} is the benefit of the feature \eqn{s} in the planning unit \eqn{i}
#' after doing actions on it (value between 0 and 1), \eqn{r_{is}} is the amount of
#' feature \eqn{s} in planning unit \eqn{i}.
#' And, the \eqn{t_s} is the target for feature \eqn{s}. The first term is the objective
#' function and the second is the set of constraints.
#'
#' In the case of working with the presence/absence of threats (binary intensities),
#' we defined the benefit as a measure of the number of actions taken against the threats that
#' affect said feature with respect to all possible actions to do. However, this may
#' underestimate the true value of benefits. For this, we use a power (`curve` parameter)
#' to raise the expression and thus seek to perform a higher density of actions per site.
#' Because we work with linear models, we use the piecewise linearization strategy to
#' work with this expression. The `segments` parameter indicates how well the expression
#' approximates. A higher number implies a better approximation but increases the resolution complexity.
#' For more information on its calculation, see the
#' [getBenefit](https://prioriactions.github.io/prioriactions/reference/getBenefit.html)
#' reference.
#'
#' Connectivity parameters (`blm` and `blm_actions`) manipulate the
#' spatial fragmentation of planning units and/or action management to improve
#' the compactness of the reserve solutions.
#'
#' @seealso For more information regarding the arguments
#'  `curve` and `segments`, see the supplementary material
#'  of Salgado-Rojas *et al.* (2020), which can
#'  be found online at <https://doi.org/10.1016/j.ecolmodel.2019.108901>.
#'
#' @examples
#' ## This example uses input files included into package.
#'
#' ## set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create optimization model
#' problem_model <- maximizeBenefits(x = problem_data, blm = 1, budget = 100)

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
  features <- features[, "internal_id"]
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

    for(i in seq_len(length(model$xvar)))
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
